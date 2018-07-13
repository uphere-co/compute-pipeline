{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.Compute where

import           Control.Concurrent                        (forkIO)
import           Control.Concurrent.STM                    (newTVarIO)
import           Control.DeepSeq                           (NFData,deepseq)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,getSelfPid
                                                           ,newChan,sendChan,receiveChan
                                                           ,send)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Data.Binary                               (Binary)
import           Data.Typeable                             (Typeable)
import           Network.Transport                         (closeTransport)
--
import           CloudHaskell.QueryQueue                   (QQVar,emptyQQ,singleQuery)
import           CloudHaskell.Type                         (LogProcess,Q(..),R(..))
import           CloudHaskell.Util                         (server,tellLog
                                                           ,expectSafe
                                                           ,withHeartBeat
                                                           ,tryCreateTransport
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (queryWorker)


singleServerProcess ::
       forall query result.
       (Binary query, Binary result, Typeable query, Typeable result, NFData result) =>
       ProcessId
    -> (query -> LogProcess result)
    -> LogProcess ()
singleServerProcess them handle = do
  (sq :: SendPort q, rq :: ReceivePort q) <- newChan
  us <- getSelfPid
  send them us
  tellLog "sent our main pid"
  send them sq
  -- send them (us,sq)
  tellLog "sent SendPort Query"
  esr <- lift expectSafe
  case esr of
    Left err' -> tellLog err'
    Right (sr :: SendPort r) -> do
      tellLog "receive SendPortResult"
      forever $ do
        q <- receiveChan rq
        r <- handle q
        r `deepseq` sendChan sr r

test :: Q -> LogProcess R
test _ = pure R


start :: () -> QQVar ComputeQuery ComputeResult -> LogProcess ()
start () qqvar = do
  ethem_ping <- lift expectSafe
  case ethem_ping of
    Left err -> tellLog err
    Right them_ping -> do
      tellLog ("got client ping pid : " ++ show them_ping)
      withHeartBeat them_ping $ \them_main -> do
        singleServerProcess them_main (liftIO . singleQuery qqvar)


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
