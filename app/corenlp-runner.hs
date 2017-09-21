module Main where

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Data.Default
import           Language.Java         as J
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
--
import           Pipeline.Load
import           Pipeline.Run.CoreNLP
import           Pipeline.Type

main :: IO ()
main = do
  [src] <- getArgs
  cfg <- (\ec -> case ec of {Left err -> error err;Right c -> return c;}) =<< loadConfigFile "config/config.json"
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

    runCoreNLPforNewsAPISource pp cfg src
