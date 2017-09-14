{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
import           Language.Java                         as J

import           System.Environment                           (getEnv)
import           System.FilePath                              ((</>))
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           MWE.NamedEntity
import           NewsAPI.DB                                   (uploadAnalysis)
import qualified NewsAPI.DB.Article                    as Ar
import           NLP.Type.CoreNLP

import           SRL.Analyze                                  (loadConfig)
import           SRL.Analyze.CoreNLP                          (preRunParser,runParser)
import           WikiEL.EntityLinking
import           WikiEL.Misc
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Operation.DB
import           Pipeline.Run.WikiEL
import           Pipeline.Type
import           Pipeline.Util


storeParsedArticles :: [Maybe (Ar.ArticleH,NewsAPIArticleContent)] -> FilePath -> IO ()
storeParsedArticles articles savepath = do
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
      -- (txt,xs) <- preRunForTaggingNE pp emTagger txt' -- Necessary for pre-running of CoreNLP
      fchk <- doesHashNameFileExistInPrefixSubDirs (savepath </> (T.unpack hsh))
      when (not fchk) $ do
        eresult <- try $ runParser pp txt
        case eresult of
          Left  (_e :: SomeException) -> return ()
          Right result               -> do
            saveHashNameBSFileInPrefixSubDirs (savepath </> (T.unpack hsh)) (BL.toStrict $ A.encode result)
            uploadAnalysis conn (mkNewsAPIAnalysisDB (DoneAnalysis (Just True) Nothing Nothing) article)

-- | Parse and Save
-- This runs CoreNLP for a specific source from NewsAPI scrapper, and save the result.
runCoreNLPforNewsAPISource :: String -> IO ()
runCoreNLPforNewsAPISource src = do
  articles <- getTimeTitleDescFromSrcWithHash src
  storeParsedArticles articles "/home/modori/data/newsapianalyzed"

-- | Pre-run of CoreNLP for changing named entity with special rule.
preRunForTaggingNE :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                   -> ([NERToken] -> [EntityMention Text])
                   -> Text -> IO (Text,[(Int,Int)])
preRunForTaggingNE pp emTagger txt = do
  sents <- preRunParser pp txt
  let wikiel = getWikiResolvedMentions emTagger sents
      constraint = mkConstraintFromWikiEL wikiel
  getReplacedTextWithNewWikiEL sents constraint
