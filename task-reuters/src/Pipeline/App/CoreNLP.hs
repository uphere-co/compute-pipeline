{-# LANGUAGE OverloadedStrings #-}
module Pipeline.App.CoreNLP where

import           Control.Concurrent                (threadDelay)
import           Control.Lens                      ((&),(^.),(.~))
import           Control.Monad                     (forever)
import qualified Data.ByteString.Char8 as B
import           Data.Default                      (def)
import           Language.Java         as J
import           System.Environment                (getEnv)
--
import           CoreNLP.Simple                    (prepare)
import           CoreNLP.Simple.Type               (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Run.CoreNLP              (runCoreNLPforRSS)


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
      -- TODO: reuters/Archive should be separated out as a configuration
      runCoreNLPforRSS pp cfg (Just "reuters/Archive",Nothing)
      putStrLn "Waiting next run..."
      -- TODO: this time should be configured.
      let sec = 1000000 in threadDelay (600*sec)

  closeConnection conn
