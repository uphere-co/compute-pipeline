{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisRunner where

import           Control.Exception                      (SomeException,handle)
import           Control.Lens
import           Control.Monad                          (forM_,when)
import           Data.Char                              (isSpace)
import           Data.List                              (zip6)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple             (Connection)
import           System.FilePath                        ((</>),takeFileName,takeBaseName)
--
import           Data.Range                             (Range)
import           Data.Time.Clock                        (UTCTime,getCurrentTime)
import           Data.Graph.Algorithm.Basic             (maxConnectedNodes,numberOfIsland)
import           DB.Operation                           (updateRSSAnalysisStatus)
import           DB.Util                                (b16ToBstrHash)
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
import           SRL.Analyze.SentenceStructure          (docStructure,mkWikiList)
import           SRL.Analyze.Type
-- import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.Type                            (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Type
import           Pipeline.Util



isSRLFiltered sstr mg =
 let a = numberOfPredicate sstr
     b = numberOfMGVerbPredicate mg
     mgraph = getGraphFromMG mg
     (c,d) = case mgraph of
               Nothing -> (-1,-1)
               Just gr -> (maxConnectedNodes gr, numberOfIsland gr)
 in ((a == b) && (c >=4) && (d < 3))


mkMGs :: Connection
      -> AnalyzePredata
      -> ([Sentence] -> [EntityMention Text])
      -> PathConfig
      -> FilePath
      -> UTCTime
      -> DocAnalysisInput
      -> IO ()
mkMGs conn apredata netagger cfg fp tm article = do
  let filename = takeFileName fp
      dstr = docStructure apredata netagger article
      sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      netags = leftTagPos (dstr^.ds_mergedtags)
      mgs = map meaningGraph sstrs
      arbs = map (mkARB (apredata^.analyze_rolemap)) mgs
      wikilsts = map mkWikiList sstrs
      isNonFilter = False
  putStrLn $ "Analyzing " ++ filename
  saveMGs (cfg ^. mgstore) filename mgs -- Temporary solution
  forM_ (zip6 ([1..] :: [Int]) sstrs mtokss mgs arbs wikilsts) $ \(i,sstr,mtks,mg,arb,wikilst) -> do
    when (isSRLFiltered sstr mg || isNonFilter) $ do
      putStrLn $ filename ++ " is filtered in!"
      putStrLn $ filename ++ ": saving MGS"      
      saveMG (cfg ^. mgstore) filename i mg
      putStrLn $ filename ++ ": saving ARB"            
      saveARB (cfg ^. arbstore) filename i (tm,(arb,netags))
      putStrLn $ filename ++ ": saving DOT"                  
      genMGFigs cfg filename i sstr mtks mg wikilst

genMGFigs :: PathConfig -> FilePath -> Int -> SentStructure -> [Maybe Token] -> MeaningGraph -> [(Range, Text)] -> IO ()
genMGFigs cfg filename i sstr mtks mg wikilst = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing -> return ()
    Just graph -> do
      let mg' = tagMG mg wikilst
      mkMGDotFigs (cfg ^. mgdotfigstore) i filename mtks mg'

runAnalysisByChunks :: Connection -> ([Sentence] -> [EntityMention Text])
                    -> AnalyzePredata -> PathConfig -> [(FilePath,UTCTime,DocAnalysisInput)] -> IO ()
runAnalysisByChunks conn netagger apredata cfg loaded = do
  flip mapM_ loaded $ \(fp,tm,artl) -> do
    handle (\(e :: SomeException) -> print e) $ do
      updateRSSAnalysisStatus conn (b16ToBstrHash (takeBaseName fp)) (Nothing,Just True,Nothing)
      mkMGs conn apredata netagger cfg fp tm artl
