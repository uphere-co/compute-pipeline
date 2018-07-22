{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}
module SemanticParserAPI.Compute where

import           Control.Distributed.Process               (Closure,Process,processNodeId)
import           Control.Distributed.Process.Closure       (mkClosure,mkStatic)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect,getSelfPid
                                                           ,newChan,sendChan,receiveChan
                                                           ,spawnChannel,spawn,send)
import           Control.Distributed.Process.Node          (newLocalNode,runProcess)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Distributed.Static
import           Control.Exception                         (bracket)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T  (unpack)
import           Network.Transport                         (closeTransport)
--
import           CloudHaskell.Server                       (server,serverUnit,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,Q(..),R(..)
                                                           ,TCPPort(..),Router(..))
import           CloudHaskell.Util                         (tellLog
                                                           ,expectSafe
                                                           ,ioWorker
                                                           ,tryCreateTransport
                                                           ,spawnChannelLocalSend
                                                           ,spawnChannelLocalDuplex
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Task       {-     (rtable
                                                           ,sdictInt
                                                           ,sdictInt__static
                                                           ,holdState
                                                           ,holdState__sdict
                                                           ,holdState__static) -}
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)


import           Data.Binary
import           Control.Distributed.Process.Internal.Closure.BuiltIn

{-
-- NOTE: This should not be type-checked! it's Int -> Closure (Process ())
test3 :: String -> Closure (Process ())
test3 = (closure (holdState__static `staticCompose` staticDecode holdState__sdict)) . encode --  @ Int

test4 :: Static (BL.ByteString -> Process ())
test4 = holdState__static `staticCompose` staticDecode holdState__sdict

test :: Static (SerializableDict Int)
test = holdState__sdict


test' :: Static (BL.ByteString -> Int)
test' = staticDecode test
-}

class Capture a where
  capture :: a -> Closure a

instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode

instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode


(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

(@<) :: (Capture a) => Closure (a -> b) -> a -> Closure b
(@<) c = closureApply c . capture

holdState__closure :: Closure (String -> Int -> Process ())
holdState__closure = staticClosure $(mkStatic 'holdState)


test__closure :: Closure (Int -> Process ())
test__closure = holdState__closure @< "abc"
                -- NOTE: equivalently
                -- holdState__closure @@ (capture @String "abc")

-- test2 :: Double
-- test2 = holdState__static

{-
test :: Double -- String -> Closure (Process ())
test = $(mkClosure 'holdState)
-}

dummyProcess :: Q -> Pipeline R
dummyProcess _ = pure R


requestHandler :: (SendPort ComputeQuery, ReceivePort ComputeResult) -> Pipeline ()
requestHandler (sq,rr) = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got client ping pid : " ++ show them_ping)
  withHeartBeat them_ping $ \them_main -> do

    (slock0,pid0) <-
      spawnChannelLocalSend $ \rlock0 ->
        serverUnit rlock0 $ \q -> do
          sendChan sq q
          receiveChan rr
    (slock1,pid1) <-
      spawnChannelLocalSend $ \rlock1 ->
        serverUnit rlock1 dummyProcess


    let router = Router $
                   HM.insert "test"  pid1 $
                   HM.insert "query" pid0 $
                   HM.empty
    send them_main router
    sendChan slock0 ()
    sendChan slock1 ()
    () <- expect  -- wait indefinitely
    pure ()


taskManager :: Pipeline ()
taskManager = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping $ \them_main -> do
    tellLog "taskManager: inside heartbeat"
    let nid = processNodeId them_main
    tellLog $ "node id = " ++ show nid
    -- us <- getSelfPid
    -- (sr :: SendPort Int,rr :: ReceivePort Int) <- newChan
    -- (sr,rr) <- newChan
    -- sq <- spawnChannel ($(mkStatic 'sdictInt)) nid ($(mkClosure 'holdState) ("3" :: String))

    -- sendChan sq 100
    -- n <- receiveChan rr

    spawn nid (test__closure @< 3)
    -- n :: Int <- expect
    -- liftIO $ print n
    () <- expect
    pure ()

initDaemonAndServer :: TCPPort -> (Bool,Bool) -> FilePath -> Process ()
initDaemonAndServer port (bypassNER,bypassTEXTNER) lcfg = do
  ((sq,rr),_) <- spawnChannelLocalDuplex $ \(rq,sr) ->
    ioWorker (rq,sr) (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)
  server port (requestHandler (sq,rr)) taskManager


computeMain :: (TCPPort,Text,Text)
            -> (Bool,Bool)  -- ^ (bypassNER, bypassTEXTNER)
            -> FilePath -- ^ configjson "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
            -> IO ()
computeMain (bcastport,hostg,hostl) (bypassNER,bypassTEXTNER) lcfg = do
    let chport = show (unTCPPort (bcastport+1))
        dhpp = DHPP (T.unpack hostg,chport) (T.unpack hostl,chport)
    bracket
            (tryCreateTransport dhpp)
            closeTransport
            (\transport ->
                    newLocalNode transport rtable
                >>= \node -> runProcess node
                               (initDaemonAndServer bcastport (bypassNER,bypassTEXTNER) lcfg)
            )
