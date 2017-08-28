{-# LANGUAGE FlexibleContexts #-}

module Pipeline.App.AnalysisRunner where

import           Control.Lens
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           System.FilePath       ((</>),takeExtension)
--
import           SRL.Analyze
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

wikiEL emTagger sents = getWikiResolvedMentions emTagger sents

saveWikiEL fp wikiel = do
  B.writeFile (fp ++ ".wiki") (BL.toStrict $ A.encode wikiel)

runAnalysis' :: IO ()
runAnalysis' = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getAnalysisFilePath
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") fps)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ loaded $ \(fp,x) -> do
    saveWikiEL fp (wikiEL emTagger (x ^. _1))
    print $ wikiEL emTagger (x ^. _1)

runAnalysis :: IO ()
runAnalysis = do
  fps' <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  let fps = filter (\x -> takeExtension x == ".wiki") fps'
  loaded <- loadWikiELResult fps
  flip mapM_ loaded $ \(fp,x) -> do
    print (fp,x)


