{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemanticParserAPI.Compute where

import           Control.Concurrent.STM                    (TVar,atomically
                                                           ,newTVarIO,readTVarIO
                                                           ,readTVar,writeTVar)
import           Control.Distributed.Process               (Process,processNodeId)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect,getSelfPid,send
                                                           ,newChan,sendChan,receiveChan)
import           Control.Distributed.Process.Node          (newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Lens                              ((&),(.~),(^.),at,to)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T  (unpack)
import           Network.Transport                         (closeTransport)
--
import           CloudHaskell.Closure                      ((@<),spawnChannel_)
import           CloudHaskell.Server                       (server,serverUnit,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,TCPPort(..),Router(..))
import           CloudHaskell.Util                         (tellLog
                                                           ,expectSafe
                                                           ,ioWorker
                                                           ,tryCreateTransport
                                                           ,spawnChannelLocalSend
                                                           ,spawnChannelLocalDuplex
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Task            (rtable,holdState__closure)
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..)
                                                           ,ComputeResult(..))
import           SemanticParserAPI.Compute.Type.Status     (Status
                                                           ,StatusQuery(..)
                                                           ,StatusResult(..)
                                                           ,statusNodes)
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)



statusQuery ::
     TVar Status
  -> StatusQuery
  -> Pipeline StatusResult
statusQuery ref _ = do
  m <- liftIO $ readTVarIO ref
  pure (SR (m^.statusNodes.to HM.toList))


requestHandler ::
     TVar Status
  -> (SendPort ComputeQuery, ReceivePort ComputeResult)
  -> Pipeline ()
requestHandler ref (sq,rr) = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got client ping pid : " ++ show them_ping)
  withHeartBeat them_ping (const (pure ())) $ \them_main -> do

    (slock0,pid0) <-
      spawnChannelLocalSend $ \rlock0 ->
        serverUnit rlock0 $ \q -> do
          sendChan sq q
          receiveChan rr
    (slock1,pid1) <-
      spawnChannelLocalSend $ \rlock1 ->
        serverUnit rlock1 (statusQuery ref)


    let router = Router $
                   HM.insert "test"  pid1 $
                   HM.insert "query" pid0 $
                   HM.empty
    send them_main router
    sendChan slock0 ()
    sendChan slock1 ()
    () <- expect  -- wait indefinitely
    pure ()


taskManager :: TVar Status -> Pipeline ()
taskManager ref = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (const (pure ())) $ \them_main -> do
    themaster <- getSelfPid
    let router = Router $ HM.insert "master" themaster mempty
    send them_main router
    cname :: Text <- expectSafe
    tellLog $ "taskManager: got " ++ show cname
    let nid = processNodeId them_main
    tellLog $ "node id = " ++ show nid
    (sr,rr) <- newChan
    sq <- spawnChannel_ nid (holdState__closure @< 0 @< sr)
    sendChan sq (100 :: Int)
    n <- receiveChan rr
    liftIO $ print n
    sendChan sq (100 :: Int)
    n' <- receiveChan rr
    liftIO $ print n'
    liftIO $ atomically $ do
      m <- readTVar ref
      -- let m' = HM.update (const (Just True)) cname m
      let m' = m & (statusNodes . at cname .~ Just True)
      writeTVar ref m'
    () <- expect
    pure ()

initDaemonAndServer :: TVar Status -> TCPPort -> (Bool,Bool) -> FilePath -> Process ()
initDaemonAndServer ref port (bypassNER,bypassTEXTNER) lcfg = do
  ((sq,rr),_) <- spawnChannelLocalDuplex $ \(rq,sr) ->
    ioWorker (rq,sr) (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)
  server port (requestHandler ref (sq,rr)) (taskManager ref)


computeMain :: Status
            -> (TCPPort,Text,Text)
            -> (Bool,Bool)  -- ^ (bypassNER, bypassTEXTNER)
            -> FilePath -- ^ configjson "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
            -> IO ()
computeMain stat (bcastport,hostg,hostl) (bypassNER,bypassTEXTNER) lcfg = do
    ref <- liftIO $ newTVarIO stat
    let chport = show (unTCPPort (bcastport+1))
        dhpp = DHPP (T.unpack hostg,chport) (T.unpack hostl,chport)
    bracket
            (tryCreateTransport dhpp)
            closeTransport
            (\transport ->
                    newLocalNode transport rtable
                >>= \node -> runProcess node
                               (initDaemonAndServer ref bcastport (bypassNER,bypassTEXTNER) lcfg)
            )
