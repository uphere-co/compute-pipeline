{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute where

import           Control.Concurrent                        (forkIO,forkOn,forkOS)
import           Control.Concurrent.STM                    (newTVarIO)
import           Control.DeepSeq                           (NFData,deepseq)
import           Control.Distributed.Process               (Process)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect,getSelfPid
                                                           ,newChan,sendChan,receiveChan
                                                           ,spawnLocal
                                                           ,send)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Monad                             (forever,void)
import           Control.Monad.IO.Class                    (liftIO)
import           Data.Binary                               (Binary)
import qualified Data.IntMap                        as IM
import           Data.Typeable                             (Typeable)
import           Network.Transport                         (closeTransport)
--
import           CloudHaskell.QueryQueue                   (QQVar,emptyQQ,singleQuery)
import           CloudHaskell.Server                       (server,serverUnit,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,Q(..),R(..))
import           CloudHaskell.Util                         (Router(..)
                                                           ,tellLog
                                                           ,expectSafe
                                                           ,ioWorker
                                                           ,tryCreateTransport
                                                           ,spawnChannelLocalSend
                                                           ,spawnChannelLocalDuplex
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)



dummyProcess :: Q -> Pipeline R
dummyProcess _ = pure R


start :: (SendPort ComputeQuery, ReceivePort ComputeResult) -> Pipeline ()
start (sq,rr) = do
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


    let router = Router $ IM.insert 1 pid1 $ IM.insert 0 pid0 $  IM.empty
    send them_main router
    sendChan slock0 ()
    sendChan slock1 ()
    () <- expect  -- wait indefinitely
    pure ()


initDaemonAndServer :: String -> (Bool,Bool) -> FilePath -> Process ()
initDaemonAndServer port (bypassNER,bypassTEXTNER) lcfg = do
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
                               (initDaemonAndServer port (bypassNER,bypassTEXTNER) lcfg)
            )
