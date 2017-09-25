{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisRunner where

import           Control.Lens
import           Control.Monad                          (forM_,when)
import           Data.Char                              (isSpace)
import           Data.List                              (zip5)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple             (Connection)
import           System.FilePath                        ((</>),takeFileName)
--
import           Data.Range                             (Range)
import           Data.Time.Clock                        (UTCTime,getCurrentTime)
import           Lexicon.Data                           (loadLexDataConfig)
import           MWE.Util                               (mkTextFromToken)
import           NewsAPI.DB
import           NLP.Shared.Type                        (PathConfig
                                                        ,arbstore,corenlpstore,lexconfigpath
                                                        ,mgstore,mgdotfigstore)
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity                   (NamedEntityClass)
import           NLP.Type.TagPos                        (leftTagPos)
import           SRL.Analyze
import           SRL.Analyze.ARB                        (mkARB)
import           SRL.Analyze.Match                      (changeMGText,meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure          (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.EntityLinking                   (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Type
import           Pipeline.Util



isSRLFiltered sstr mg =
 let a = numberOfPredicate sstr
     b = numberOfMGVerbPredicate mg
     mgraph = getGraphFromMG mg
     (c,d) = case mgraph of
               Nothing -> (-1,-1)
               Just gr -> (farthestPath gr, numberOfIsland gr)
 in ((a == b) && (c >=4) && (d < 3))


mkMGs :: Connection
      -> AnalyzePredata
      -> ([Sentence] -> [EntityMention Text])
      -> PathConfig
      -> FilePath
      -> DocAnalysisInput
      -> IO ()
mkMGs conn apredata netagger cfg fp article = do
  let filename = takeFileName fp
      dstr = docStructure apredata netagger article
      sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      netags = leftTagPos (dstr^.ds_mergedtags)
      mgs = map meaningGraph sstrs
      arbs = map (mkARB (apredata^.analyze_rolemap)) mgs
      wikilst = SRLWiki.mkWikiList dstr
      isNonFilter = False
  saveMGs (cfg ^. mgstore) filename mgs -- Temporary solution
  forM_ (zip5 ([1..] :: [Int]) sstrs mtokss mgs arbs) $ \(i,sstr,mtks,mg,arb) -> do
    when (isSRLFiltered sstr mg || isNonFilter) $ do
      saveMG (cfg ^. mgstore) filename i mg
      ctime <- getCurrentTime
      saveARB (cfg ^. arbstore) filename i (ctime,(arb,netags))
      genMGFigs cfg filename i sstr mtks mg wikilst
  -- updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)

genMGFigs :: PathConfig -> FilePath -> Int -> SentStructure -> [Maybe Token] -> MeaningGraph -> [(Range, Text)] -> IO ()
genMGFigs cfg filename i sstr mtks mg wikilst = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing -> return ()
    Just graph -> do
      let mg' = tagMG mg wikilst
      mkMGDotFigs (cfg ^. mgdotfigstore) i filename mtks mg'

runAnalysisAll :: PathConfig -> Connection -> IO ()
runAnalysisAll cfg conn = do
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger) <- loadConfig cfgG
  -- (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) 
  -- let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  as <- getAllAnalysisFilePath cfg
  loaded' <- loadCoreNLPResult $ map (\(fp,tm) -> ((cfg ^. corenlpstore) </> fp, tm)) as
  let loaded = catMaybes $ map (\(a,b,c) -> (,,) <$> Just a <*> Just b <*> c) loaded'
  flip mapM_ loaded $ \(fp,tm,x) -> do
    mkMGs conn apredata netagger cfg fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysisByChunks :: Connection -> ([Sentence] -> [EntityMention Text])
                    -> AnalyzePredata -> PathConfig -> [(FilePath,UTCTime,DocAnalysisInput)] -> IO ()
runAnalysisByChunks conn netagger apredata cfg loaded = do
  flip mapM_ loaded $ \(fp,artl) -> do
    mkMGs conn apredata netagger cfg fp artl
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)
