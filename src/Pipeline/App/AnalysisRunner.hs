{-# LANGUAGE FlexibleContexts #-}

module Pipeline.App.AnalysisRunner where

import qualified Data.Aeson            as A
import           Data.Maybe
import           System.FilePath       ((</>))
--
import           OntoNotes.App.Analyze
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

wikiEL emTagger loaded = getWikiResolvedMentions emTagger loaded

saveWikiEL wikiel = do
  A.encode wikiel

runAnalysis :: IO ()
runAnalysis = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getAnalysisFilePath
  loaded <- catMaybes <$> loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") fps)
  flip mapM_ loaded $ \x -> do
    print $ wikiEL emTagger x
