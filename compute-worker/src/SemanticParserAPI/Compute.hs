{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.Compute where

import           Control.Concurrent                        (forkIO)
import           Control.Concurrent.STM                    (newTVarIO)
import           Control.DeepSeq                           (deepseq)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect
                                                           ,newChan,sendChan,receiveChan
                                                           ,send,spawnLocal)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Network.Transport                         (closeTransport)
import           System.IO                                 (hPutStrLn,stderr)
--
import           CloudHaskell.QueryQueue                   (QQVar,emptyQQ,singleQuery)
import           CloudHaskell.Util                         (LogProcess,server,tellLog
                                                           ,expectSafe
                                                           ,withHeartBeat
                                                           ,tryCreateTransport)
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (queryWorker)


start :: () -> QQVar ComputeQuery ComputeResult -> LogProcess ()
start () qqvar = do
  ethem <- lift expectSafe
  case ethem of
    Left err -> tellLog err
    Right them -> do
      tellLog ("got client pid : " ++ show them)

      withHeartBeat them $ spawnLocal $ do
        (sc,rc) <- newChan :: LogProcess (SendPort (ComputeQuery, SendPort ComputeResult), ReceivePort (ComputeQuery, SendPort ComputeResult))
        send them sc

        tellLog "connected"
        forever $ do
          (q,sc') <- receiveChan rc
          spawnLocal $ do
            r <- liftIO $ singleQuery qqvar q
            r `deepseq` sendChan sc' r


computeMain :: (Int,String,String)
            -> (Bool,Bool)  -- ^ (bypassNER, bypassTEXTNER)
            -> FilePath -- ^ configjson "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
            -> IO ()
computeMain (portnum,hostg,hostl) (bypassNER,bypassTEXTNER) lcfg = do
    let port = show portnum
        port' = show (portnum+1)
        dhpp = DHPP (hostg,port') (hostl,port')
    qqvar <- liftIO (newTVarIO emptyQQ)

    forkIO $
      bracket (tryCreateTransport dhpp)
              closeTransport
              (\transport ->
                      newLocalNode transport initRemoteTable
                  >>= \node -> runProcess node (server qqvar port start ())
              )
    queryWorker (bypassNER,bypassTEXTNER) lcfg qqvar
