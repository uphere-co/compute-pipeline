module SemanticParserAPI.Compute where

-- import           Control.Concurrent.STM
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Loops                       (whileJust_)
-- import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode)
-- import qualified Data.HashMap.Strict                 as HM
import           Data.Monoid                               ((<>))
-- import           Data.Text                                 (Text)
import           Foreign.C.String
import           Network.Transport.UpHere                  (createTransport,defaultTCPParameters
                                                           ,DualHostPortPair(..))
import           Options.Applicative
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

data ServerOption = ServerOption { _port :: Int
                                 , _hostg :: String
                                 , _hostl :: String
                                 , _config :: String
                                 , _corenlp :: String
                                 }


pOptions :: Parser ServerOption
pOptions = ServerOption <$> option auto (long "port" <> short 'p' <> help "Port number")
                        <*> strOption (long "global-ip" <> short 'g' <> help "Global IP address")
                        <*> strOption (long "local-ip"  <> short 'l' <> help "Local IP address")
                        <*> strOption (long "config-file" <> short 'c' <> help "Config file")
                        <*> strOption (long "corenlp" <> short 'n' <> help "CoreNLP server address")


queryServerOption :: ParserInfo ServerOption
queryServerOption = info pOptions ( fullDesc <> progDesc "Query server daemon" <> header "options are port, global-ip, local-ip, config-file, corenlp")


computeMain :: IO ()
computeMain = do
  opt <- execParser queryServerOption

  let portnum = _port opt
      -- port = show portnum
      port' = show (portnum+1)
      hostg = _hostg opt
      hostl = _hostl opt
      config = _config opt
      -- corenlp_server = _corenlp opt
      dhpp = DHPP (hostg,port') (hostl,port')

  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> hPutStrLn stderr (show err)
    Right transport -> do
      _node <- newLocalNode transport initRemoteTable
      withCString config $ \_configfile -> do
        print "hello"
        -- engine <- newEngineWrapper configfile
        -- runProcess node (server port (start corenlp_server) engine)
        -- deleteEngineWrapper engine
