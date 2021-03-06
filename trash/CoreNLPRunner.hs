module Pipeline.App.CoreNLPRunner where

import           Control.Concurrent                (threadDelay)
import           Control.Lens                      ((&),(^.),(.~))
import           Control.Monad                     (forever,forM_)
import qualified Data.ByteString.Char8 as B
import           Data.Default                      (def)
import qualified Data.Text             as T
import           Language.Java         as J
import           System.Environment                (getEnv)
--
import           CoreNLP.Simple                    (prepare)
import           CoreNLP.Simple.Type               (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           NLP.Shared.Type                   (PathConfig,dbstring)
import           RSS.Data                          (rssAnalysisList)
--
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Run.CoreNLP              (runCoreNLPforRSS)
import           Pipeline.Type                     (SourceConstraint(..))

runCoreNLP :: PathConfig -> SourceConstraint -> IO ()
runCoreNLP cfg sc = do
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
    runCoreNLPforRSS pp cfg sc
  closeConnection conn
