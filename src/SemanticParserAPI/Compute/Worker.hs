{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

module SemanticParserAPI.Compute.Worker where

import           Control.Concurrent.STM         (atomically,retry,modifyTVar',readTVar,writeTVar)
import           Control.Lens                   ((&),(^.),(^..),(.~),_Just,makeLenses)
import           Control.Monad                  (forever)
import qualified Data.ByteString.Char8  as B
import           Data.Default                   (def)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap            as IM
import           Data.Maybe                     (catMaybes)
import           Data.Text                      (Text)
import qualified Data.Text              as T
import           Data.Tree                      (Forest)
import qualified Language.Java          as J
import           System.Environment             (getEnv)
--
import           CoreNLP.Simple                 (prepare)
import           CoreNLP.Simple.Type            (tokenizer,words2sentences,postagger
                                                ,lemma,sutime,constituency,ner)
import           Lexicon.Data                   (loadLexDataConfig)
import           NER.Type                       (CompanyInfo)
import           NLP.Shared.Type                (PathConfig(..))
import           NLP.Syntax.Format              (formatX'Tree)
import           NLP.Syntax.Type.XBar           (SPhase(SPH1),lemmaList)
import           NLP.Type.CoreNLP               (Sentence)
import           SRL.Analyze                    (loadConfig,consoleOutput)
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP            (runParser)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure  (docStructure,mkWikiList)
import           SRL.Analyze.Type               (AnalyzePredata,DocStructure,MeaningGraph
                                                ,analyze_framedb
                                                ,ds_mtokenss,ds_sentStructures,ss_tagged
                                                ,ss_x'trs
                                                )
import           WikiEL.Type                    (EntityMention)
--
import           CloudHaskell.QueryQueue        (QQVar,QueryStatus(..),next)
import           CloudHaskell.Util              (tellLog)
import           SemanticParserAPI.Compute.Reuters (loadExistingMG)
import           SemanticParserAPI.Compute.Type (ComputeQuery(..)
                                                ,ComputeResult(..)
                                                ,ResultSentence(..)
                                                ,ResultReuters(..))

data SRLData = SRLData { _aconfig :: Analyze.Config
                       , _pipeline :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                       , _apredata :: AnalyzePredata
                       , _netagger :: ([Sentence] -> [EntityMention Text])
                       , _forest :: Forest (Either Int Text)
                       , _companyMap :: IntMap CompanyInfo
                       }

makeLenses ''SRLData

allMeaningGraphs :: AnalyzePredata -> IntMap CompanyInfo -> DocStructure -> [MeaningGraph]
allMeaningGraphs apdat cmap dstr =
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map (\sstr -> (sstr,meaningGraph apdat sstr)) sstrs1
  in flip map (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(_mtks,(_i,(sstr,mg))) ->
       let wikilst = mkWikiList cmap sstr
       in tagMG mg wikilst


runSRL :: SRLData -> Text -> IO ([[(Int,Text)]],[MeaningGraph],Text)
runSRL sdat sent = do
  dainput <- runParser (sdat^.pipeline) sent
  dstr <- docStructure (sdat^.apredata) (sdat^.netagger) (sdat^.forest,sdat^.companyMap) dainput
  let sstrs = dstr ^.. ds_sentStructures . traverse . _Just
      tokenss = map (map (\(x,(_,y)) -> (x,y))) $ sstrs ^.. traverse . ss_tagged . lemmaList
      mgs = allMeaningGraphs (sdat^.apredata) (sdat^.companyMap) dstr
      outputtxt = consoleOutput (sdat^.apredata.analyze_framedb) dstr

  return (tokenss,mgs,outputtxt)


{-
runReuters :: Int -> IO [MeaningGraph]
runReuters n = do

  return "no results"
-}

queryWorker :: (Bool,Bool)
            -> FilePath
            -> QQVar ComputeQuery ComputeResult
            -> IO ()
queryWorker (bypassNER,bypassTEXTNER) lcfg qqvar = do
  let acfg  = Analyze.Config False False bypassNER bypassTEXTNER lcfg
  cfg <- loadLexDataConfig (acfg^. Analyze.configFile) >>= \case Left err -> error err
                                                                 Right x  -> return x
  (apdat,ntggr,frst,cmap) <- SRL.Analyze.loadConfig (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER) cfg
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )

    let sdat = SRLData { _aconfig = acfg
                       , _pipeline = pp
                       , _apredata = apdat
                       , _netagger = ntggr
                       , _forest = frst
                       , _companyMap = cmap
                       }
    forever $ do
      (i,q) <- atomically $ do
                 qq <- readTVar qqvar
                 case next qq of
                   Nothing -> retry
                   Just (i,q) -> do
                     let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                     writeTVar qqvar qq'
                     return (i,q)
      case q of
        CQ_Sentence txt -> do
          (tokenss,mgs,otxt) <- runSRL sdat txt
          let r = CR_Sentence (ResultSentence txt tokenss mgs otxt)
          atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
        CQ_Reuters n -> do
          putStrLn ("CQ_Reuters " ++ show n)
          lst <- catMaybes <$> loadExistingMG testPathConfig n
          -- mapM_ print lst
          print (length lst)
          -- rtxt <- runReuters n
          let r = CR_Reuters (ResultReuters n lst)
          atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
          return ()


testPathConfig :: PathConfig
testPathConfig = PathConfig
  { _corenlpstore  = "/scratch/wavewave/run_webapp/data/newsapianalyzed"
  , _mgstore       = "/scratch/wavewave/run_webapp/temp/mgs"
  , _mgdotfigstore = "/scratch/wavewave/run_webapp/temp/dot"
  , _lexconfigpath = "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
  , _arbstore      = "/scratch/wavewave/run_webapp/temp/arb"
  , _errstore      = "/scratch/wavewave/run_webapp/data/newsapierror"
  , _dbstring      = "dbname=mydbexp host=localhost port=5432 user=wavewave"
  , _newsapistore  = ""
  , _nytstore      = ""
  , _rssstore      = "/scratch/wavewave/run_webapp/RSS"
  }
