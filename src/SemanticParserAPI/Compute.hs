{-# LANGUAGE LambdaCase          #-}
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
import           Control.Lens                              ((^.),(.~),(&))
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.ByteString.Char8               as B
import           Data.Default                              (def)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Language.Java                       as J
import           Network.Transport                         (closeTransport)
import           System.Environment                        (getEnv)
import           System.IO                                 (hPutStrLn,stderr)
--
import           CoreNLP.Simple                            (prepare)
import           CoreNLP.Simple.Type                       (tokenizer,words2sentences,postagger
                                                           ,lemma,sutime,constituency,ner)
import           Lexicon.Data                              (loadLexDataConfig)
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           SRL.Analyze                               (loadConfig)
import qualified SRL.Analyze.Config                  as Analyze
--
import           CloudHaskell.Util                         (LogProcess,server,tellLog
                                                           ,withHeartBeat,tryCreateTransport)
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..),ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (SRLData(..),queryWorker)


start :: SRLData -> TMVar (HM.HashMap Text ([Int],[Text])) -> LogProcess ()
start sdat resultref = do
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
      (q,sc') <- receiveChan rc
      -- liftIO $ hPutStrLn stderr (show q)
      -- sendChan sc' (CR_Text txt)
      spawnLocal (queryWorker sdat resultref sc' q)




computeMain :: (Int,String,String) -> IO ()
computeMain (portnum,hostg,hostl) = do
  let acfg  = Analyze.Config False False bypassNER bypassTEXTNER "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
      bypassNER = True -- False
      bypassTEXTNER = True -- False
  cfg  <- loadLexDataConfig (acfg^. Analyze.configFile) >>= \case Left err -> error err
                                                                  Right x  -> return x
  (apdat,ntggr,frst,cmap) <- SRL.Analyze.loadConfig (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER) cfg
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )

    let srldata = SRLData { _aconfig = acfg
                          , _pipeline = pp
                          , _apredata = apdat
                          , _netagger = ntggr
                          , _forest = frst
                          , _companyMap = cmap
                          }
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
            (\transport -> newLocalNode transport initRemoteTable >>= \node -> runProcess node (server port start srldata))

        -- withCString config $ \_configfile -> do
          -- engine <- newEngineWrapper configfile
          -- deleteEngineWrapper engine
