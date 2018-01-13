module SemanticParserAPI.Compute where

-- import           Control.Concurrent.STM
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Loops                       (whileJust_)
-- import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode)
-- import qualified Data.HashMap.Strict                 as HM
-- import           Data.Monoid                               ((<>))
-- import           Data.Text                                 (Text)
-- import           Foreign.C.String
import           Network.Transport.UpHere                  (createTransport,defaultTCPParameters
                                                           ,DualHostPortPair(..))
-- import           Options.Applicative
-- import           System.Environment
import           System.IO                                 (hPutStrLn, stderr)
--
-- import           CloudHaskell.Server
-- import           Network.Util
-- import           SemanticParserAPI.Compute.Worker

{-
start :: String -> EngineWrapper -> TMVar (HM.HashMap Text ([Int],[Text])) -> LogProcess ()
start corenlp_server engine resultref = do
  them :: ProcessId <- expect
  tellLog ("got client pid : " ++ show them)
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
      -- port = show portnum
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
      _node <- newLocalNode transport initRemoteTable
      print "hello"
      
      -- withCString config $ \_configfile -> do
        -- engine <- newEngineWrapper configfile
        -- runProcess node (server port (start corenlp_server) engine)
        -- deleteEngineWrapper engine
