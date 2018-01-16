{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.Compute where

import           Control.Concurrent                        (forkIO,forkOn,forkOS,threadDelay)
import           Control.Concurrent.STM                    (TMVar,atomically,retry,newTVarIO,readTVar,writeTVar)
import           Control.DeepSeq                           (deepseq)
import           Control.Exception                         (bracket)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect
                                                           ,newChan,sendChan,receiveChan
                                                           ,send,spawnLocal)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Lens                              ((^.),(.~),(&))
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.IntMap                         as IM
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Language.Java                       as J
import           Network.Transport                         (closeTransport)
import           System.IO                                 (hPutStrLn,stderr)
--
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import qualified SRL.Analyze.Config                  as Analyze
--
import           CloudHaskell.QueryQueue                   (QQVar,emptyQQ,getAnswered,newQuery,remove,singleQuery)
import           CloudHaskell.Util                         (LogProcess,server,tellLog
                                                           ,withHeartBeat,tryCreateTransport)
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (SRLData(..),queryWorker)


start :: () -> QQVar ComputeQuery ComputeResult -> LogProcess ()
start () qqvar = do
  them :: ProcessId <- expect
  tellLog ("got client pid : " ++ show them)

  withHeartBeat them $ spawnLocal $ do
    (sc,rc) <- newChan :: LogProcess (SendPort (ComputeQuery, SendPort ComputeResult), ReceivePort (ComputeQuery, SendPort ComputeResult))
    send them sc

    liftIO $ hPutStrLn stderr "connected"
    forever $ do
      (q,sc') <- receiveChan rc
      spawnLocal $ do
        r <- liftIO $ singleQuery qqvar q
        -- liftIO (print r) -- for the time being to make sure r is fully evaluated.
        r `deepseq` sendChan sc' r


computeMain :: (Int,String,String) -> IO ()
computeMain (portnum,hostg,hostl) = do
    let port = show portnum
        port' = show (portnum+1)
        dhpp = DHPP (hostg,port') (hostl,port')
    qqvar <- liftIO (newTVarIO emptyQQ)
    
    forkIO $
      bracket (tryCreateTransport dhpp)
              closeTransport
              (\transport -> newLocalNode transport initRemoteTable >>= \node -> runProcess node (server qqvar port start ()))
    queryWorker qqvar
