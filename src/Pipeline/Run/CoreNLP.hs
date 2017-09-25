{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.Run.CoreNLP where

import           Control.Exception                            (SomeException,try)
import           Control.Lens                                 ((^.))
import           Control.Monad                                (forM_,when)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Maybe
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import           Language.Java                         as J
import           System.FilePath                              ((</>))
--
import           MWE.NamedEntity
import           NewsAPI.DB                                   (uploadAnalysisIfMissing,uploadArticleError)
import qualified NewsAPI.DB.Article                    as Ar
import           NLP.Shared.Type                              (PathConfig,corenlpstore,dbstring,errstore)
import           NLP.Type.CoreNLP
import           RSS.DB
import qualified RSS.DB.Article                        as RAr
import           SRL.Analyze.CoreNLP                          (preRunParser,runParser)
import           WikiEL.EntityLinking
import           WikiEL.Run
--
import           Pipeline.Source.NewsAPI.Article
import qualified Pipeline.Source.RSS.Article           as RSS
import           Pipeline.Operation.DB
-- import           Pipeline.Run.WikiEL
import           Pipeline.Type
import           Pipeline.Util

storeParsedArticles :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                    -> PathConfig
                    -> [Maybe (Ar.ArticleH,NewsAPIArticleContent)]
                    -> IO ()
storeParsedArticles pp cfg articles = do
  conn <- getConnection (cfg ^. dbstring)
  forM_ (catMaybes articles) $ \(article,(hsh,_,_,txt)) -> do
    -- (txt,xs) <- preRunForTaggingNE pp emTagger txt' -- Necessary for pre-running of CoreNLP
    fchk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. corenlpstore) </> (T.unpack hsh))
    echk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. errstore) </> (T.unpack hsh))
    when (not fchk && not echk) $ do
      eresult <- try $ runParser pp txt
      case eresult of
        Left  (_e :: SomeException) -> do
          saveHashNameTextFileInPrefixSubDirs ((cfg ^. errstore) </> (T.unpack hsh)) txt
          uploadArticleError conn (mkNewsAPIArticleErrorDB article)
        Right result                -> do
          saveHashNameBSFileInPrefixSubDirs ((cfg ^. corenlpstore) </> (T.unpack hsh)) (BL.toStrict $ A.encode result)
          uploadAnalysisIfMissing conn (mkNewsAPIAnalysisDB (DoneAnalysis (Just True) Nothing Nothing) article)
  closeConnection conn


storeParsedRSSArticles :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                       -> PathConfig
                       -> [Maybe (RAr.RSSArticleH,RSS.RSSArticleContent)]
                       -> IO ()
storeParsedRSSArticles pp cfg articles = do
  conn <- getConnection (cfg ^. dbstring)
  forM_ (catMaybes articles) $ \(article,(hsh,_,_,txt)) -> do
    fchk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. corenlpstore) </> (T.unpack hsh))
    echk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. errstore) </> (T.unpack hsh))
    when (not fchk && not echk) $ do
      eresult <- try $ runParser pp txt
      case eresult of
        Left  (_e :: SomeException) -> do
          saveHashNameTextFileInPrefixSubDirs ((cfg ^. errstore) </> (T.unpack hsh)) txt
        Right result                -> do
          saveHashNameBSFileInPrefixSubDirs ((cfg ^. corenlpstore) </> (T.unpack hsh)) (BL.toStrict $ A.encode result)
          uploadRSSAnalysisIfMissing conn (mkRSSAnalysisDBInfo (DoneAnalysis (Just True) Nothing Nothing) article)
  closeConnection conn



-- | Parse and Save
-- This runs CoreNLP for a specific source from NewsAPI scrapper, and save the result.
runCoreNLPforNewsAPISource :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> PathConfig -> String -> IO ()
runCoreNLPforNewsAPISource pp cfg src = do
  articles <- getTimeTitleDescFromSrcWithHash cfg src
  storeParsedArticles pp cfg articles

runCoreNLPforRSS :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> PathConfig -> String -> IO ()
runCoreNLPforRSS pp cfg src = do
  articles <- RSS.getTimeTitleDescFromSrcWithHash cfg src
  storeParsedRSSArticles pp cfg articles


{-
-- | Pre-run of CoreNLP for changing named entity with special rule.
preRunForTaggingNE :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                   -> ([NERToken] -> [EntityMention Text])
                   -> Text -> IO (Text,[(Int,Int)])
preRunForTaggingNE pp emTagger txt = do
  sents <- preRunParser pp txt
  let wikiel = getWikiResolvedMentions emTagger sents
      constraint = mkConstraintFromWikiEL wikiel
  getReplacedTextWithNewWikiEL sents constraint
-}
