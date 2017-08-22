{-# LANGUAGE FlexibleContexts #-}

module Pipeline.App.AnalysisRunner where

import           Data.Maybe
--
import           OntoNotes.App.Analyze
--
import           Pipeline.Load
import           Pipeline.Run

runWiki loaded emTagger = do
  print (getWikiResolvedMentions loaded emTagger)

runAnalysis :: IO ()
runAnalysis = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  loaded <- catMaybes <$> loadCoreNLPResult "/home/modori/data/newsapianalyzed"
  flip mapM_ loaded $ \x -> do
    runWiki x emTagger
