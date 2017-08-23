{-# LANGUAGE FlexibleContexts #-}

module Pipeline.App.AnalysisRunner where

import           Data.Maybe
--
import           OntoNotes.App.Analyze
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

runWiki loaded emTagger = do
  print (getWikiResolvedMentions loaded emTagger)

runAnalysis :: IO ()
runAnalysis = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getAnalysisFilePath 
  loaded <- catMaybes <$> loadCoreNLPResult (map ((++) "/home/modori/data/newsapianalyzed") fps)
  flip mapM_ loaded $ \x -> do
    runWiki x emTagger
