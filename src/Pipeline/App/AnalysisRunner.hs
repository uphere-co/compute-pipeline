{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.App.AnalysisRunner where

import           Control.Lens
import           Control.Monad                          (forM_,void,when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                              (zip4)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                  as T
import           System.Directory                       (withCurrentDirectory)
import           System.FilePath                        ((</>),takeExtension,takeFileName)
import           System.Process                         (readProcess)
--
import           MWE.Util
import           NewsAPI.DB                             (getAnalysisBySource)
import qualified NewsAPI.DB.Analysis        as Analysis
import           NLP.Type.CoreNLP
import           SRL.Analyze
import           SRL.Analyze.Format                     (dotMeaningGraph)
import           SRL.Analyze.Match                      (meaningGraph)
import           SRL.Analyze.SentenceStructure          (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           Text.Format.Dot                        (mkLabelText)
import           WikiEL.EntityLinking                   (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Operation.DB                  (getConnection)
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

wikiEL emTagger sents = getWikiResolvedMentions emTagger sents

saveWikiEL fp wikiel = B.writeFile (fp ++ ".wiki") (BL8.toStrict $ A.encode wikiel)

mkMGs apredata emTagger fp loaded = do
  let filename = takeFileName fp
  let dstr = docStructure apredata emTagger loaded
  let sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map meaningGraph sstrs
      wikilst = SRLWiki.mkWikiList dstr
      
  forM_ (zip4 [1..] sstrs mtokss mgs) $ \(i,sstr,mtks,mg') -> do
    when (numberOfPredicate sstr == numberOfMGPredicate mg') $ do
      let mgraph = getGraphFromMG mg'
      case mgraph of
        Nothing -> return ()
        Just graph -> do
          when (furthestPath graph >= 4 && numberOfIsland graph < 3) $ do
            let title = mkTextFromToken mtks  
                mg = tagMG mg' wikilst
            let dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
            putStrLn dotstr
            withCurrentDirectory "/home/modori/data/meaning_graph" $ do
              writeFile (filename ++ "_" ++ (show i) ++ ".dot") dotstr
              void (readProcess "dot" ["-Tpng",filename ++ "_" ++ (show i) ++ ".dot","-o"++ filename ++ "_" ++ (show i) ++ ".png"] "")

runAnalysisAll :: IO ()
runAnalysisAll = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats

  as <- getAnalysisFilePathBySource "bloomberg"
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ (take 100 loaded) $ \(fp,x) -> do
    mkMGs apredata emTagger fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysisByChunks :: ([NERToken] -> [EntityMention Text])
                    -> AnalyzePredata -> [(FilePath,DocAnalysisInput)] -> IO ()
runAnalysisByChunks emTagger apredata loaded = do
  flip mapM_ loaded $ \(fp,x) -> do
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
