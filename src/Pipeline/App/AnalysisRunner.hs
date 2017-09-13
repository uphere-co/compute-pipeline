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
import           Database.PostgreSQL.Simple             (Connection)
import           System.Directory                       (doesFileExist)
import           System.FilePath                        ((</>),takeExtension,takeFileName)
import           System.Process                         (readProcess)
import           Text.Printf                            (printf)
--
import           MWE.Util
import           NewsAPI.DB
import qualified NewsAPI.DB.Analysis        as Analysis
import           NLP.Type.CoreNLP
import           SRL.Analyze
import           SRL.Analyze.Match                      (meaningGraph)
import           SRL.Analyze.SentenceStructure          (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.EntityLinking                   (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Operation.DB                  (getConnection)
import           Pipeline.Run
import           Pipeline.Run.SRL
import           Pipeline.Run.WikiEL
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Util

mkMGs conn apredata emTagger fp loaded = do

  let filename = takeFileName fp
      dstr = docStructure apredata emTagger loaded
      sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map meaningGraph sstrs
      wikilst = SRLWiki.mkWikiList dstr
      isNonFilter = False

  -- saveMG "/home/modori/temp/mgs" filename mgs

  forM_ (zip4 [1..] sstrs mtokss mgs) $ \(i,sstr,mtks,mg') -> do
    -- fchk <- doesFileExist ("/home/modori/data/meaning_graph/" ++ filename ++ "_" ++ (show i) ++ ".png")
    -- when (not fchk) $ do
    when (numberOfPredicate sstr == numberOfMGPredicate mg' || isNonFilter) $ do
      let mgraph = getGraphFromMG mg'
      case mgraph of
        Nothing -> return ()
        Just graph -> do
          when ((furthestPath graph >= 4 && numberOfIsland graph < 3) || isNonFilter) $ do
            let mg = tagMG mg' wikilst
            print $ mkTextFromToken mtks
            print mg
            mkARB mg
            genMGFigs "/home/modori/data/meaning_graph" i filename mtks mg
            updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)
            

runAnalysisAll :: Connection -> IO ()
runAnalysisAll conn = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats

  as <- getAnalysisFilePathBySource "bloomberg"
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ (take 100 loaded) $ \(fp,x) -> do
    mkMGs conn apredata emTagger fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysisByChunks :: Connection -> ([NERToken] -> [EntityMention Text])
                    -> AnalyzePredata -> [(FilePath,DocAnalysisInput)] -> IO ()
runAnalysisByChunks conn emTagger apredata loaded = do
  flip mapM_ loaded $ \(fp,x) -> do
    mkMGs conn apredata emTagger fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysis :: IO ()
runAnalysis = do
  fps' <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  let fps = filter (\x -> takeExtension x == ".wiki") fps'
  loaded <- loadWikiELResult fps
  flip mapM_ loaded $ \(fp,x) -> do
    print (fp,x)
