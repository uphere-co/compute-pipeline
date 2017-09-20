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
import           Data.Time.Clock                        (getCurrentTime)
import           Lexicon.Data                           (loadLexDataConfig)
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
      arbs = map mkARB mgs 
      wikilst = SRLWiki.mkWikiList dstr
      isNonFilter = False
  -- saveMGs "/home/modori/temp/mgs" filename mgs -- Temporary solution
  forM_ (zip5 ([1..] :: [Int]) sstrs mtokss mgs arbs) $ \(i,sstr,mtks,mg,arb) -> do
    when (isSRLFiltered sstr mg || isNonFilter) $ do
      -- saveMG "/home/modori/temp/mgs" filename i mg
      -- ctime <- getCurrentTime
      -- saveARB "/home/modori/temp/arb" filename i (ctime,arb)
      genMGFigs filename i sstr mtks mg wikilst
  -- updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)

genMGFigs :: FilePath -> Int -> SentStructure -> [Maybe Token] -> MeaningGraph -> [(Range, Text)] -> IO ()
genMGFigs filename i sstr mtks mg wikilst = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing -> return ()
    Just graph -> do
        let mg' = tagMG mg wikilst
        mkMGDotFigs "/home/modori/data/meaning_graph" i filename mtks mg'

runAnalysisAll :: Connection -> IO ()
runAnalysisAll conn = do
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig "/home/modori/repo/src/lexicon-builder/config_global.json"
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig cfgG
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
