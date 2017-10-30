module Pipeline.App.CoreNLPDaemon where

import           Control.Concurrent                (threadDelay)
import           Control.Lens                      ((&),(^.),(.~))
import           Control.Monad                     (forever,forM_)
import qualified Data.ByteString.Char8 as B
import           Data.Default                      (def)
import           Language.Java         as J
import           System.Environment                (getEnv)
--
import           CoreNLP.Simple                    (prepare)
import           CoreNLP.Simple.Type               (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           NLP.Shared.Type                   (PathConfig,dbstring)
import           RSS.Data                          (rssList)
--
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Run.CoreNLP              (runCoreNLPforRSS)

runDaemon :: PathConfig -> String -> String -> IO ()
runDaemon cfg src sec = do
  clspath <- getEnv "CLASSPATH"
  conn <- getConnection (cfg ^. dbstring)
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    forever $ do
      forM_ rssList $ \(src,sec,url) -> runCoreNLPforRSS pp cfg (src ++ "/" ++ sec)
      putStrLn "Waiting next run..."
      let sec = 1000000 in threadDelay (60*sec)

  closeConnection conn
