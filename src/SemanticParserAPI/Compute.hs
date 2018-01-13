{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.Compute where

import           Control.Concurrent                        (threadDelay)
import           Control.Concurrent.STM                    (TMVar)
import           Control.Exception                         (bracket)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect
                                                           ,newChan,sendChan,receiveChan
                                                           ,send,spawnLocal)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import           Network.Transport                         (closeTransport)
import           System.IO                                 (hPutStrLn,stderr)
--
import           Network.Transport.UpHere                  (DualHostPortPair(..))
--
import           CloudHaskell.Util                         (LogProcess,server,tellLog
                                                           ,withHeartBeat,tryCreateTransport)
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))


start :: () -> TMVar (HM.HashMap Text ([Int],[Text])) -> LogProcess ()
start () _resultref = do
  them :: ProcessId <- expect
  tellLog ("got client pid : " ++ show them)

  withHeartBeat them $ spawnLocal $ do
    {- liftIO $ forever $ do
      threadDelay 10000000
      putStrLn "running"
    -}
    (sc,rc) <- newChan :: LogProcess (SendPort (ComputeQuery, SendPort ComputeResult), ReceivePort (ComputeQuery, SendPort ComputeResult))
    send them sc
    liftIO $ hPutStrLn stderr "connected"
    forever $ do
      (CQ_Text txt,sc') <- receiveChan rc
      -- liftIO $ hPutStrLn stderr (show q)
      sendChan sc' (CR_Text txt)
      -- spawnLocal (queryWorker resultref sc' q)




computeMain :: (Int,String,String) -> IO ()
computeMain (portnum,hostg,hostl) = do
  let -- portnum = _port opt
      port = show portnum
      port' = show (portnum+1)
      -- hostg = _hostg opt
      -- hostl = _hostl opt
      -- config = _config opt
      -- corenlp_server = _corenlp opt
      dhpp = DHPP (hostg,port') (hostl,port')
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport -> newLocalNode transport initRemoteTable >>= \node -> runProcess node (server port start ()))

      -- withCString config $ \_configfile -> do
        -- engine <- newEngineWrapper configfile
        -- deleteEngineWrapper engine
