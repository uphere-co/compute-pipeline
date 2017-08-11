{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Language.Java         as J
import qualified Data.ByteString.Char8 as B
import System.Environment              (getArgs,getEnv)
import           Data.Default
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
--
import  Pipeline.Application.CoreNLPParser

main :: IO ()
main = do
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
    result <- runCoreNLPParser "Hello" pp
    print result
