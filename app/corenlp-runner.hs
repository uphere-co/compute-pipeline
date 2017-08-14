{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson            as A
import           Control.Lens
import           Language.Java         as J
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import System.Environment              (getArgs,getEnv)
import           Data.Default
import qualified Data.Text.IO   as TIO
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
--
import  Pipeline.Application.CoreNLPParser
import  Pipeline.Batch

main' :: IO ()
main' = do
  result <- readAndParse
  print result
  
main :: IO ()
main = do
  txt <- TIO.readFile "data.txt"
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
    results <- runNewsAPIbatch $ flip runCoreNLPParser pp
    print results
    -- BL.writeFile "result.txt" (A.encode result)
