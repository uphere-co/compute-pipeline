{-# LANGUAGE FlexibleContexts #-}

module Pipeline.App.AnalysisRunner where

import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           System.FilePath       ((</>))
--
import           OntoNotes.App.Analyze
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

wikiEL emTagger loaded = getWikiResolvedMentions emTagger loaded

saveWikiEL fp wikiel = do
  B.writeFile (fp ++ ".wiki") (BL.toStrict $ A.encode wikiel)

runAnalysis :: IO ()
runAnalysis = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getAnalysisFilePath
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") fps)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ loaded $ \(fp,x) -> do
    saveWikiEL fp (wikiEL emTagger x)
    print $ wikiEL emTagger x
