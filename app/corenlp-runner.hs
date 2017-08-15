{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad              (forM_,replicateM_,void)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List.Split            (chunksOf)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Foreign.JNI                as J
import           Language.Java              as J
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           OntoNotes.App.Analyze
--
import           Pipeline.Application.CoreNLPParser
import           Pipeline.Batch
import           Pipeline.Load
import           Pipeline.Source.NewsAPI.Article

main'' :: IO ()
main'' = loadCoreNLPResult "/home/modori/data/newsapianalyzed" >>= print

main' :: IO ()
main' = do
  (sensemap,sensestat,framedb,ontomap,emTagger) <- loadConfig
  loaded' <- loadCoreNLPResult "/home/modori/data/newsapianalyzed"
  let loaded = catMaybes loaded'
  forM_ loaded $ \x -> do
    sentStructure' sensemap sensestat framedb ontomap emTagger x


main :: IO ()
main = do
  str <- newEmptyMVar
  articles' <- getTimeTitleDescFromSrcWithHash "bloomberg"

  void $ forkOS (runCoreNLP (take 1000 articles') str)
  void $ forkOS (runCoreNLP (drop 1000 articles') str)
  replicateM_ 2 (takeMVar str >>= putStrLn)

runCoreNLP articles str = do
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
    forM_ (catMaybes articles) $ \(hsh,_,_,x) -> do
      result <- runCoreNLPParser x pp
      BL.writeFile ("/home/modori/data/newsapianalyzed/" ++ (T.unpack hsh)) (A.encode result)
    putMVar str ("Done!")
