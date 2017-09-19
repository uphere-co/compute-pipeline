{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisRunner where

import           Control.Lens
import           Control.Monad                          (forM_,when)
import           Data.Char                              (isSpace)
import           Data.List                              (zip4)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple             (Connection)
import           System.FilePath                        ((</>),takeFileName)
--
import           Data.Range                             (Range)
import           Data.Time.Clock                        (getCurrentTime)
import           MWE.Util                               (mkTextFromToken)
import           NewsAPI.DB
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity                   (NamedEntityClass)
import           SRL.Analyze
import           SRL.Analyze.Match                      (changeMGText,meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure          (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.EntityLinking                   (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Run.SRL
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Util


mkMGs :: Connection
      -> AnalyzePredata
      -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
      -> FilePath
      -> DocAnalysisInput
      -> IO ()
mkMGs conn apredata emTagger fp article = do
  let filename = takeFileName fp
      dstr = docStructure apredata emTagger article
      sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map meaningGraph sstrs
      arb = map mkARB mgs 
      wikilst = SRLWiki.mkWikiList dstr
      isNonFilter = False
--  print mgs
--  genARB mgs
  ctime <- getCurrentTime
  saveMG "/home/modori/temp/mgs" filename mgs
  saveARB "/home/modori/temp/arb" filename (ctime,arb)
  genMGFigs filename sstrs mtokss mgs wikilst isNonFilter
  updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)

genMGFigs :: FilePath -> [SentStructure] -> [[Maybe Token]] -> [MeaningGraph] -> [(Range, Text)] -> Bool -> IO ()
genMGFigs filename sstrs mtokss mgs wikilst isNonFilter = do
  forM_ (zip4 ([1..] :: [Int]) sstrs mtokss mgs) $ \(i,sstr,mtks,mg') -> do
    when (numberOfPredicate sstr == numberOfMGVerbPredicate mg' || isNonFilter) $ do
      let mgraph = getGraphFromMG mg'
      case mgraph of
        Nothing -> return ()
        Just graph -> do
          when ((farthestPath graph >= 4 && numberOfIsland graph < 3) || isNonFilter) $ do
            let mg = tagMG mg' wikilst
                mg'' = changeMGText mg
            mkMGDotFigs "/home/modori/data/meaning_graph" i filename mtks mg''

genARB :: [MeaningGraph] -> IO ()
genARB mgs = forM_ mgs $ \mg -> print $ mkARB mg
  
genOrigSents :: [[Maybe Token]] -> IO ()
genOrigSents mtokss = forM_ mtokss $ \mtks -> putStrLn $ (T.unpack $ T.dropWhile isSpace $ mkTextFromToken mtks)

runAnalysisAll :: Connection -> IO ()
runAnalysisAll conn = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  as <- getAllAnalysisFilePath
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  flip mapM_ (take 100 loaded) $ \(fp,x) -> do
    mkMGs conn apredata emTagger fp x
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)

runAnalysisByChunks :: Connection -> ([NERToken] -> [EntityMention Text])
                    -> AnalyzePredata -> [(FilePath,DocAnalysisInput)] -> IO ()
runAnalysisByChunks conn emTagger apredata loaded = do
  flip mapM_ loaded $ \(fp,artl) -> do
    mkMGs conn apredata emTagger fp artl
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)
