{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad              (forM_,when)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Maybe                 (catMaybes)
import qualified Data.Text.IO               as TIO
import           Language.Java              as J
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           MWE
import           OntoNotes.App.Analyze
import           OntoNotes.App.Analyze.SentenceStructure
import           WikiEL.EntityLinking
import           WikiEL.Misc
--
import           Pipeline.App.CoreNLPRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Util
 
-- Load and Run
main'' :: IO ()
main'' = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  loaded' <- loadCoreNLPResult fps
  putStrLn "Loading Completed."
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  forM_ loaded $ \(fp,x) -> do
    mapM_ TIO.putStrLn (sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats x)

-- Parse and Save
main' :: IO ()
main' = do
  [src] <- getArgs
  articles <- getTimeTitleDescFromSrcWithHash src
  runCoreNLP articles

main :: IO ()
main = do
  txt <- TIO.readFile "test2.txt"
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  sents <- preRunCoreNLP txt
  let wikiel = getWikiResolvedMentions emTagger sents
      constraint = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ wikiel
  preProcessing sents constraint

