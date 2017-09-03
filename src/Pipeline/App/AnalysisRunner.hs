{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.App.AnalysisRunner where

import           Control.Lens
import           Control.Monad         (forM_,void)
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import qualified Data.Text             as T
import           System.FilePath       ((</>),takeExtension,takeFileName)
import           System.Process        (readProcess)
--
import           MWE.Util
import           NLP.Type.CoreNLP
import           SRL.Analyze
import           SRL.Analyze.Format            (dotMeaningGraph)
import           SRL.Analyze.Match             (meaningGraph)
import           SRL.Analyze.SentenceStructure (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL    as SRLWiki
import           Text.Format.Dot               (mkLabelText)
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

wikiEL emTagger sents = getWikiResolvedMentions emTagger sents

saveWikiEL fp wikiel = do
  B.writeFile (fp ++ ".wiki") (BL.toStrict $ A.encode wikiel)

mkMGs apredata emTagger fp loaded = do
  let filename = takeFileName fp
  let dstr = docStructure apredata emTagger loaded
  let sstrs1 = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map meaningGraph sstrs1
      wikilst = SRLWiki.mkWikiList dstr
      
  forM_ (zip mtokss (zip [1..] mgs)) $ \(mtks,(i,mg')) -> do
    let title = mkTextFromToken mtks  
        mg = tagMG mg' wikilst
    let dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
    putStrLn dotstr
    writeFile (filename ++ "_" ++ (show i) ++ ".dot") dotstr
    void (readProcess "dot" ["-Tpng",filename ++ "_" ++ (show i) ++ ".dot","-o"++ filename ++ "_" ++ (show i) ++ ".png"] "")

runAnalysis' :: IO ()
runAnalysis' = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  fps <- getAnalysisFilePath
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") fps)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ (take 300 loaded) $ \(fp,x) -> do
    mkMGs apredata emTagger fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysis :: IO ()
runAnalysis = do
  fps' <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  let fps = filter (\x -> takeExtension x == ".wiki") fps'
  loaded <- loadWikiELResult fps
  flip mapM_ loaded $ \(fp,x) -> do
    print (fp,x)
