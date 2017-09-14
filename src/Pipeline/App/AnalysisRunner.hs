{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisRunner where

import           Control.Concurrent.Async               (async,wait)
import           Control.Exception                      (SomeException,try)
import           Control.Lens
import           Control.Monad                          (forM_,when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                              (zip4)
import           Data.Maybe
import           Data.Text                              (Text)
import           Database.PostgreSQL.Simple             (Connection,close)
import           System.FilePath                        ((</>),takeExtension,takeFileName)
--
import           NewsAPI.DB
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity                   (NamedEntityClass)
import           SRL.Analyze
import           SRL.Analyze.Match                      (meaningGraph)
import           SRL.Analyze.SentenceStructure          (docStructure)
import           SRL.Analyze.Type
import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.EntityLinking                   (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Operation.DB
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
      wikilst = SRLWiki.mkWikiList dstr
      isNonFilter = False

  saveMG "/home/modori/temp/mgs" filename mgs
  updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)

  {-
  forM_ (zip4 ([1..] :: [Int]) sstrs mtokss mgs) $ \(_i,sstr,_,mg') -> do
    when (numberOfPredicate sstr == numberOfMGPredicate mg' || isNonFilter) $ do
      let mgraph = getGraphFromMG mg'
      case mgraph of
        Nothing -> return ()
        Just graph -> do
          when ((furthestPath graph >= 4 && numberOfIsland graph < 3) || isNonFilter) $ do
            let mg = tagMG mg' wikilst
            mkARB mg
            -- genMGFigs "/home/modori/data/meaning_graph" i filename mtks mg
            updateAnalysisStatus conn (unB16 filename) (Nothing, Just True, Nothing)
  -}

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

runAnalysisByChunks :: ([NERToken] -> [EntityMention Text])
                    -> AnalyzePredata -> [(FilePath,DocAnalysisInput)] -> IO ()
runAnalysisByChunks emTagger apredata loaded = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  flip mapM_ loaded $ \(fp,artl) -> do
    mkMGs conn apredata emTagger fp artl
    -- saveWikiEL fp (wikiEL emTagger (x ^. dainput_sents))
    -- print $ wikiEL emTagger (x ^. dainput_sents)
  close conn
