{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.Compute where

import           Control.Concurrent.STM                    (TMVar)
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Loops                       (whileJust_)
import           Control.Distributed.Process               (ProcessId)
import           Control.Distributed.Process.Lifted        (expect)
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import qualified Data.HashMap.Strict                 as HM
-- import           Data.Monoid                               ((<>))
import           Data.Text                                 (Text)
import           Network.Transport.UpHere                  (createTransport,defaultTCPParameters
                                                           ,DualHostPortPair(..))
-- import           Options.Applicative
-- import           System.Environment
import           System.IO                                 (hPutStrLn, stderr)
--
import           CloudHaskell.Server                       (LogProcess,server,tellLog)
-- import           Network.Util
-- import           SemanticParserAPI.Compute.Worker


start :: () -> TMVar (HM.HashMap Text ([Int],[Text])) -> LogProcess ()
start () _resultref = do
  them :: ProcessId <- expect
  tellLog ("got client pid : " ++ show them)
{-
  withHeartBeat them $ spawnLocal $ do
    (sc,rc) <- newChan :: LogProcess (SendPort (Query, SendPort ResultBstr), ReceivePort (Query, SendPort ResultBstr))
    send them sc
    liftIO $ hPutStrLn stderr "connected"
    forever $ do
      (q,sc') <- receiveChan rc
      liftIO $ hPutStrLn stderr (show q)
      -- spawnLocal (queryWorker corenlp_server resultref sc' engine q)
-}


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

  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> hPutStrLn stderr (show err)
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runProcess node (server port start ())

      -- withCString config $ \_configfile -> do
        -- engine <- newEngineWrapper configfile
        -- deleteEngineWrapper engine
