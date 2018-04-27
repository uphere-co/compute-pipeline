{-# LANGUAGE OverloadedStrings #-}
module Pipeline.App.CoreNLPDaemon where

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
--
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Run.CoreNLP              (runCoreNLPforRSS)
import           Pipeline.Type                     (SourceTimeConstraint(..))
import           Pipeline.Util                     (digitsToUTC)

{-
srcOnlyConst src sec = SourceConstraint (Just (T.pack $ src ++ "/" ++ sec)) Nothing Nothing
srcBTConst src sec = SourceConstraint (Just (T.pack $ src ++ "/" ++ sec)) Nothing Nothing -- (digitsToUTC "20170501") (digitsToUTC "20171110")
-}

runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
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
      runCoreNLPforRSS pp cfg (Just "reuters/Archive",Nothing)
      putStrLn "Waiting next run..."
      let sec = 1000000 in threadDelay (60*sec)

  closeConnection conn
