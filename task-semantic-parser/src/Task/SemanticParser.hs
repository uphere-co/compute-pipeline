{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Task.SemanticParser where

import           Control.Concurrent.STM         ( TVar )
import           Control.DeepSeq                ( NFData )
import           Control.Lens                   ( (&), (^.), (^..), (.~)
                                                , _Just, makeLenses
                                                )
import           Data.Aeson                     ( FromJSON, ToJSON
                                                , eitherDecode'
                                                )
import           Data.Binary                    ( Binary )
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Default                   ( def )
import           Data.IntMap                    ( IntMap )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Language.Java          as J
import           System.Environment             ( getEnv )
------ language-engine
import           CoreNLP.Simple                 ( prepare )
import           CoreNLP.Simple.Type            ( tokenizer, words2sentences, postagger
                                                , lemma, sutime, constituency, ner
                                                )
import           NER.Type                       ( CompanyInfo )
import           NLP.Shared.Type                ( PathConfig(..) )
import           NLP.Syntax.Type.XBar           ( lemmaList )
import           SRL.Analyze                    ( loadConfig, consoleOutput )
import           SRL.Analyze.Config             ( SRLConfig )
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP            ( runParser )
import           SRL.Analyze.Match.MeaningGraph ( meaningGraph, tagMG )
import           SRL.Analyze.SentenceStructure  ( docStructure, mkWikiList )
import           SRL.Analyze.Type               ( AnalysisData
                                                , ConsoleOutput
                                                , DocStructure
                                                , MeaningGraph
                                                , SRLData
                                                , analysis_SRLData
                                                , analysis_CompanyMap
                                                , cmap_map
                                                , srldata_framedb
                                                , ds_mtokenss, ds_sentStructures
                                                , ss_tagged
                                                )
------ compute-pipeline
import           CloudHaskell.QueryQueue        ( QQVar
                                                , handleQuery
                                                , handleQueryInterrupted
                                                )
import           Task.Reuters                   ( loadExistingMG )
import           Worker.Type           ( StatusProc(..) )


type Pipeline = J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")


data ComputeQuery = CQ_Sentence Text
                  | CQ_Reuters Int
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)


data ResultSentence = ResultSentence { _sentence_query :: Text
                                     , _sentence_token :: [[(Int,Text)]]
                                     , _sentence_meaning_graph :: [MeaningGraph]
                                     , _sentence_output :: ConsoleOutput
                                     }
                    deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)


data ResultReuters = ResultReuters { _reuters_query :: Int
                                   , _reuters_mgs :: [MeaningGraph]
                                   }
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)


data ComputeResult = CR_Sentence ResultSentence
                   | CR_Reuters  ResultReuters
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)


data SemanticParserModel =
  SemanticParserModel {
    _aconfig    :: Analyze.Config
  , _pipeline   :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
  , _analysisData :: AnalysisData
  }

makeLenses ''SemanticParserModel


allMeaningGraphs ::
     SRLData
  -> IntMap CompanyInfo
  -> DocStructure
  -> [MeaningGraph]
allMeaningGraphs sdata cmap dstr =
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map (\sstr -> (sstr,meaningGraph sdata sstr)) sstrs1
  in flip map (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(_mtks,(_i,(sstr,mg))) ->
       let wikilst = mkWikiList cmap sstr
       in tagMG mg wikilst


runSRL ::
     SemanticParserModel
  -> Text
  -> IO ([[(Int,Text)]],[MeaningGraph],ConsoleOutput)
runSRL sdat sent = do
  dainput <- runParser (sdat^.pipeline) sent
  dstr <- docStructure (sdat^.analysisData) dainput
  let sstrs = dstr ^.. ds_sentStructures . traverse . _Just
      tokenss = map (map (\(x,(_,y)) -> (x,y))) $ sstrs ^.. traverse . ss_tagged . lemmaList
      mgs = allMeaningGraphs
              (sdat^.analysisData.analysis_SRLData)
              (sdat^.analysisData.analysis_CompanyMap.cmap_map)
              dstr
      cout = consoleOutput (sdat^.analysisData.analysis_SRLData.srldata_framedb) dstr

  pure (tokenss,mgs,cout)


prepareAndProcess :: (Pipeline -> IO ()) -> IO ()
prepareAndProcess action = do
  pp <- prepare (def & (tokenizer .~ True)
                     . (words2sentences .~ True)
                     . (postagger .~ True)
                     . (lemma .~ True)
                     . (sutime .~ True)
                     . (constituency .~ True)
                     . (ner .~ True)
                )
  action pp

-- TODO: use ExceptT
runSRLQueryDaemon0 ::
      (Bool,Bool)
    -> FilePath
    -> QQVar ComputeQuery ComputeResult
    -> IO ()
runSRLQueryDaemon0 (bypassNER,bypassTEXTNER) lcfg rQQ = do
  let acfg  = Analyze.Config False False bypassNER bypassTEXTNER lcfg
  cfg <- do e <- eitherDecode' @SRLConfig <$> BL.readFile (acfg ^. Analyze.configFile)
            case e of
              Left err -> error err
              Right x -> return x
  analData <- SRL.Analyze.loadConfig
                (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER) cfg
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    prepareAndProcess $ \pp -> do
      let sdat = SemanticParserModel {
                   _aconfig = acfg
                 , _pipeline = pp
                 , _analysisData = analData
                 }
      handleQuery rQQ $ \case
        CQ_Sentence txt -> do
          (tokenss,mgs,cout) <- runSRL sdat txt
          pure $ CR_Sentence (ResultSentence txt tokenss mgs cout)
        CQ_Reuters n -> do
          -- TODO: no more testPathConfig
          lst <- catMaybes <$> loadExistingMG testPathConfig n
          print (length lst)
          pure $ CR_Reuters (ResultReuters n lst)


-- TODO: use ExceptT
runSRLQueryDaemon ::
      (Bool,Bool)
    -> FilePath
    -> TVar StatusProc
    -> QQVar ComputeQuery ComputeResult
    -> IO ()
runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg rProc rQQ = do
  let acfg  = Analyze.Config False False bypassNER bypassTEXTNER lcfg
  cfg <- do e <- eitherDecode' @SRLConfig <$> BL.readFile (acfg ^. Analyze.configFile)
            case e of
              Left err -> error err
              Right x -> return x
  -- (sdata,ntggr,frst,cmap)
  analData <-
    SRL.Analyze.loadConfig
      (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER)
      cfg

  prepareAndProcess $ \pp -> do
    putStrLn "pipeline ready"
    let sdat = SemanticParserModel {
                 _aconfig = acfg
               , _pipeline = pp
               , _analysisData = analData
               }
    handleQueryInterrupted rProc rQQ $ \case
      CQ_Sentence txt -> do
        (tokenss,mgs,cout) <- runSRL sdat txt
        pure $ CR_Sentence (ResultSentence txt tokenss mgs cout)
      CQ_Reuters n -> do
        -- TODO: no more testPathConfig
        lst <- catMaybes <$> loadExistingMG testPathConfig n
        print (length lst)
        pure $ CR_Reuters (ResultReuters n lst)


-- TODO: remove this.
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
