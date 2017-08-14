{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad              (forM_)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java              as J
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
--
import           Pipeline.Application.CoreNLPParser
import           Pipeline.Batch
import           Pipeline.Source.NewsAPI.Article

main' :: IO ()
main' = do
  result <- readAndParse
  print result
  
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
    articles <- getTimeTitleDescFromSrcWithHash "bloomberg"
    forM_ (catMaybes articles) $ \(hsh,_,_,x) -> do
      result <- runCoreNLPParser x pp
      BL.writeFile ("/home/modori/data/newsapianalyzed/" ++ (T.unpack hsh)) (A.encode result)
