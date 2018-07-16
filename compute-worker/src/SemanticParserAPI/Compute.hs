{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute where

import           Control.Concurrent                        (forkIO,forkOn,forkOS) -- runInBoundThread
import           Control.Concurrent.STM                    (newTVarIO)
import           Control.DeepSeq                           (NFData,deepseq)
import           Control.Distributed.Process               (Process)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,getSelfPid
                                                           ,newChan,sendChan,receiveChan
                                                           ,spawnLocal
                                                           ,send)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Monad                             (forever,void)
import           Control.Monad.IO.Class                    (liftIO)
import           Data.Binary                               (Binary)
import           Data.Typeable                             (Typeable)
import           Network.Transport                         (closeTransport)
--
import           CloudHaskell.QueryQueue                   (QQVar,emptyQQ,singleQuery)
import           CloudHaskell.Server                       (server,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,Q(..),R(..))
import           CloudHaskell.Util                         (tellLog
                                                           ,expectSafe
                                                           ,ioWorker
                                                           ,tryCreateTransport
                                                           ,spawnChannelLocalDuplex
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)


singleServerProcess ::
       forall query result.
       (Binary query, Binary result, Typeable query, Typeable result, NFData result) =>
       ProcessId
    -> (query -> Pipeline result)
    -> Pipeline ()
singleServerProcess them handle = do
  (sq :: SendPort q, rq :: ReceivePort q) <- newChan
  us <- getSelfPid
  send them us
  tellLog "sent our main pid"
  send them sq
  -- send them (us,sq)
  tellLog "sent SendPort Query"
  sr :: SendPort r <- expectSafe
  tellLog "receive SendPortResult"
  forever $ do
    q <- receiveChan rq
    r <- handle q
    r `deepseq` sendChan sr r

test :: Q -> Pipeline R
test _ = pure R




start :: (SendPort ComputeQuery, ReceivePort ComputeResult) -> Pipeline ()
start (sq,rr) = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got client ping pid : " ++ show them_ping)
  withHeartBeat them_ping $ \them_main ->
    singleServerProcess them_main $ \q -> do
      sendChan sq q
      receiveChan rr


serverInit :: String -> (Bool,Bool) -> FilePath -> Process ()
serverInit port (bypassNER,bypassTEXTNER) lcfg = do
  ((sq,rr),_) <- spawnChannelLocalDuplex $ \(rq,sr) ->
    ioWorker (rq,sr) (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)
  server port (start (sq,rr))


computeMain :: (Int,String,String)
            -> (Bool,Bool)  -- ^ (bypassNER, bypassTEXTNER)
            -> FilePath -- ^ configjson "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
            -> IO ()
computeMain (portnum,hostg,hostl) (bypassNER,bypassTEXTNER) lcfg = do
    let port = show portnum
        port' = show (portnum+1)
        dhpp = DHPP (hostg,port') (hostl,port')
    bracket
            (tryCreateTransport dhpp)
            closeTransport
            (\transport ->
                    newLocalNode transport initRemoteTable
                >>= \node -> runProcess node
                               (serverInit port (bypassNER,bypassTEXTNER) lcfg)
            )
