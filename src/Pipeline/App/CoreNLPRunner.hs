{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.App.CoreNLPRunner where

import           Control.Exception                            (SomeException,try)
import           Control.Lens
import           Control.Monad                                (forM_,when)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as B
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Default
import           Data.Maybe
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO
import           Language.Java                         as J
import           System.Directory                             (doesFileExist)
import           System.Environment                           (getEnv)
import           System.FilePath                              ((</>))
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Util
import           MWE.NamedEntity
import           NewsAPI.DB                                   (uploadAnalysis)
import qualified NewsAPI.DB.Article                    as Ar
import           NLP.Type.CoreNLP                             (NERToken,Sentence)
import           NLP.Type.PennTreebankII
import           OntoNotes.App.Analyze
import           OntoNotes.App.Analyze.CoreNLP                (preRunParser,runParser)
import           OntoNotes.App.Analyze.SentenceStructure
import           OntoNotes.App.Util
import           Text.ProtocolBuffers.WireMessage             (messageGet)
import           WikiEL.EntityLinking
import           WikiEL.Misc
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Run
import           Pipeline.Util


runCoreNLPAndSave :: [Maybe (Ar.ArticleH,NewsAPIArticleContent)] -> FilePath -> IO ()
runCoreNLPAndSave articles savepath = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"

  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig

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
    forM_ (catMaybes articles) $ \(article,(hsh,_,_,txt)) -> do
      -- (txt,xs) <- preRunForTaggingNE pp emTagger txt'
      fchk <- doesFileExist (savepath </> (T.unpack hsh))
      when (not fchk) $ do
        eresult <- try $ runParser pp txt
        case eresult of
          Left  (e :: SomeException) -> return ()
          Right result               -> do
            saveHashNameBSFileInPrefixSubDirs (savepath </> (T.unpack hsh)) (BL.toStrict $ A.encode result)
            uploadAnalysis conn (mkNewsAPIAnalysisDB article)

-- | Load and Run
-- This loads parsed result and runs NLP analysis. The result is printed on stdout.
loadAndRunNLPAnalysis :: IO ()
loadAndRunNLPAnalysis = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  loaded' <- loadCoreNLPResult fps
  putStrLn "Loading Completed."
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  forM_ loaded $ \(fp,x) -> do
    mapM_ TIO.putStrLn (sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats x)

-- | Parse and Save
-- This runs CoreNLP for a specific source from NewsAPI scrapper, and save the result.
runCoreNLPforNewsAPISource :: String -> IO ()
runCoreNLPforNewsAPISource src = do
  articles <- getTimeTitleDescFromSrcWithHash src
  runCoreNLPAndSave articles "/home/modori/data/newsapianalyzed"

-- | Pre-run of CoreNLP for changing named entity with special rule.
preRunForTaggingNE :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                   -> ([NERToken] -> [EntityMention Text])
                   -> Text -> IO (Text,[(Int,Int)])
preRunForTaggingNE pp emTagger txt = do
  sents <- preRunParser pp txt
  let wikiel = getWikiResolvedMentions emTagger sents
      constraint = mkConstraintFromWikiEL wikiel
  getReplacedTextWithNewWikiEL sents constraint

mkConstraintFromWikiEL wikiel = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ wikiel
