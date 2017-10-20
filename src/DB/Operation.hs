{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.Operation where

import           Control.Arrow
import           Control.Lens                               ((^.))
import           Control.Monad.IO.Class                     (liftIO)
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Int                                   (Int64)
import           Data.Maybe                                 (fromJust,isNothing)
import           Data.Text                                  (Text)
import qualified Data.Text                      as T
import           Data.Time.Clock                            (UTCTime,getCurrentTime)
import           Database.PostgreSQL.Simple                 (Connection)
import           Opaleye                        hiding      (constant)
import           Opaleye.Internal.Column                    (unColumn)
--
import           Model.Opaleye.ShowConstant                 (constant,safeCoerceToRep)
import           Model.Opaleye.To
import qualified DB.Schema.RSS.Analysis         as An
import qualified DB.Schema.RSS.Article          as A
import qualified DB.Schema.RSS.ErrorArticle     as EA
--
import           DB.Type

uploadRSSArticle :: (ToRSSArticle a) => Connection -> a -> IO ()
uploadRSSArticle conn x = do
  let a = toRSSArticle x
  runInsert conn A.table $
    A.newRSSArticle (a ^. rss_article_hash) (a ^. rss_article_source) (a ^. rss_article_created)
  return ()

uploadRSSArticleIfMissing :: (ToRSSArticle a) => Connection -> a -> IO ()
uploadRSSArticleIfMissing conn x = do
  let a = toRSSArticle x
  as' <- getRSSArticleByHash conn (a ^. rss_article_hash)
  case as' of
    []  -> uploadRSSArticle conn a
    _as -> putStrLn "Already exists"

uploadRSSErrorArticle :: (ToRSSErrorArticle a) => Connection -> a -> IO ()
uploadRSSErrorArticle conn x = do
  let a = toRSSErrorArticle x
  runInsert conn EA.table $
    EA.newRSSErrorArticle (a ^. rss_error_article_hash) (a ^. rss_error_article_source) (a ^. rss_error_article_errormsg) (a ^. rss_error_article_created)
  return ()

uploadRSSErrorArticleIfMissing :: (ToRSSErrorArticle a) => Connection -> a -> IO ()
uploadRSSErrorArticleIfMissing conn x = do
  let a = toRSSErrorArticle x
  as' <- getRSSErrorArticleByHash conn (a ^. rss_error_article_hash)
  case as' of
    []  -> uploadRSSErrorArticle conn a
    _as -> putStrLn "Already exists"

uploadRSSAnalysis :: (ToRSSAnalysis a) => Connection -> a -> IO ()
uploadRSSAnalysis conn x = do
  let a = toRSSAnalysis x
  runInsert conn An.table $
    An.newRSSAnalysis (a ^. rss_analysis_hash) (a ^. rss_analysis_source) (a ^. rss_analysis_corenlp) (a ^. rss_analysis_srl) (a ^. rss_analysis_ner) (a ^. rss_analysis_created)
  return ()

uploadRSSAnalysisIfMissing :: (ToRSSAnalysis a) => Connection -> a -> IO ()
uploadRSSAnalysisIfMissing conn x = do
  let a = toRSSAnalysis x
  as' <- getRSSAnalysisByHash conn (a ^. rss_analysis_hash)
  case as' of
    []  -> uploadRSSAnalysis conn a
    _as -> putStrLn "Already exists"

countRSSArticleAll :: Query (Column PGInt8)
countRSSArticleAll = proc () -> do
  n <- aggregate count (fmap A._id queryRSSArticleAll) -< ()
  returnA -< n

countRSSArticleByTime :: UTCTime -> Query (Column PGInt8)
countRSSArticleByTime time = proc () -> do
  n <- aggregate count (fmap A._id (queryRSSArticleByTime time)) -< ()
  returnA -< n

countRSSArticleBetweenTime :: UTCTime -> UTCTime -> Query (Column PGInt8)
countRSSArticleBetweenTime time1 time2 = proc () -> do
  n <- aggregate count (fmap A._id (queryRSSArticleBetweenTime time1 time2)) -< ()
  returnA -< n

queryRSSArticleAll :: Query (To Column (A.RSSArticle))
queryRSSArticleAll = proc () -> do
  r <- A.queryAll -< ()
  returnA -< r

queryRSSArticleBetweenTime :: UTCTime -> UTCTime -> Query (To Column (A.RSSArticle))
queryRSSArticleBetweenTime time1 time2 = proc () -> do
  r <- A.queryAll -< ()
  restrict -< ((pgUTCTime time1 .<= (safeCoerceToRep $ A._created r)) .&& ((safeCoerceToRep $ A._created r) .<= pgUTCTime time2))
  returnA -< r

queryRSSArticleBySource :: String -> Query (To Column (A.RSSArticle))
queryRSSArticleBySource src = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  returnA -< r

queryRSSArticleByTime :: UTCTime -> Query (To Column (A.RSSArticle))
queryRSSArticleByTime time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r

queryRSSArticleBySourceAndTime :: String -> UTCTime -> Query (To Column (A.RSSArticle))
queryRSSArticleBySourceAndTime src time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r

queryRSSArticleByHash :: ByteString -> Query (To Column (A.RSSArticle))
queryRSSArticleByHash hsh = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._hash r .== (constant hsh)
  returnA -< r

queryRSSErrorArticleByHash :: ByteString -> Query (To Column (EA.RSSErrorArticle))
queryRSSErrorArticleByHash hsh = proc () -> do
  r <- EA.queryAll -< ()
  restrict -< EA._hash r .== (constant hsh)
  returnA -< r

countRSSAnalysisAll :: Query (Column PGInt8)
countRSSAnalysisAll = proc () -> do
  n <- aggregate count (fmap An._id queryRSSAnalysisAll) -< ()
  returnA -< n

countRSSAnalysisByTime :: UTCTime -> Query (Column PGInt8)
countRSSAnalysisByTime time = proc () -> do
  n <- aggregate count (fmap An._id (queryRSSAnalysisByTime time)) -< ()
  returnA -< n

countRSSAnalysisBetweenTime :: UTCTime -> UTCTime -> Query (Column PGInt8)
countRSSAnalysisBetweenTime time1 time2 = proc () -> do
  n <- aggregate count (fmap An._id (queryRSSAnalysisBetweenTime time1 time2)) -< ()
  returnA -< n

queryRSSAnalysisAll :: Query (To Column (An.RSSAnalysis))
queryRSSAnalysisAll = proc () -> do
  r <- An.queryAll -< ()
  returnA -< r

queryRSSAnalysisBetweenTime :: UTCTime -> UTCTime -> Query (To Column (An.RSSAnalysis))
queryRSSAnalysisBetweenTime time1 time2 = proc () -> do
  r <- An.queryAll -< ()
  restrict -< ((pgUTCTime time1 .<= (safeCoerceToRep $ An._created r)) .&& ((safeCoerceToRep $ An._created r) .<= pgUTCTime time2))
  returnA -< r

queryRSSAnalysisBySource :: String -> Query (To Column (An.RSSAnalysis))
queryRSSAnalysisBySource src = proc () -> do
  r <- An.queryAll -< ()
  restrict -< An._source r .== (constant (T.pack src))
  returnA -< r

queryRSSAnalysisByTime :: UTCTime -> Query (To Column (An.RSSAnalysis))
queryRSSAnalysisByTime time = proc () -> do
  r <- An.queryAll -< ()
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ An._created r)
  returnA -< r

queryRSSAnalysisBySourceAndTime :: String -> UTCTime -> Query (To Column (An.RSSAnalysis))
queryRSSAnalysisBySourceAndTime src time = proc () -> do
  r <- An.queryAll -< ()
  restrict -< An._source r .== (constant (T.pack src))
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ An._created r)
  returnA -< r

queryRSSAnalysisByHash :: ByteString -> Query (To Column (An.RSSAnalysis))
queryRSSAnalysisByHash hsh = proc () -> do
  r <- An.queryAll -< ()
  restrict -< An._hash r .== (constant hsh)
  returnA -< r

getRSSArticleAll :: Connection -> IO [A.RSSArticleH]
getRSSArticleAll conn = (runQuery conn queryRSSArticleAll)

getCountRSSArticleAll conn = do
  [n] <- liftIO $ (runQuery conn countRSSArticleAll :: IO [Int64])
  return n

getCountRSSArticleByTime time conn = do
  [n] <- liftIO $ (runQuery conn (countRSSArticleByTime time) :: IO [Int64])
  return n

getCountRSSArticleBetweenTime time1 time2 conn = do
  [n] <- liftIO $ (runQuery conn (countRSSArticleBetweenTime time1 time2) :: IO [Int64])
  return n

getRSSArticleBySource :: Connection -> String -> IO [A.RSSArticleH] 
getRSSArticleBySource conn src = runQuery conn (queryRSSArticleBySource src)

getRSSArticleByTime :: Connection -> UTCTime -> IO [A.RSSArticleH]
getRSSArticleByTime conn time = runQuery conn (queryRSSArticleByTime time)

getRSSArticleBySourceAndTime :: Connection -> String -> UTCTime -> IO [A.RSSArticleH]
getRSSArticleBySourceAndTime conn src time = runQuery conn (queryRSSArticleBySourceAndTime src time)

getRSSArticleByHash :: Connection -> ByteString -> IO [A.RSSArticleH]
getRSSArticleByHash conn hsh = (runQuery conn (queryRSSArticleByHash hsh) :: IO [A.RSSArticleH])

getRSSErrorArticleByHash :: Connection -> ByteString -> IO [EA.RSSErrorArticleH]
getRSSErrorArticleByHash conn hsh = (runQuery conn (queryRSSErrorArticleByHash hsh) :: IO [EA.RSSErrorArticleH])

getCountRSSAnalysisAll conn = do
  [n] <- liftIO $ (runQuery conn countRSSAnalysisAll :: IO [Int64])
  return n

getCountRSSAnalysisByTime time conn = do
  [n] <- liftIO $ (runQuery conn (countRSSAnalysisByTime time) :: IO [Int64])
  return n

getCountRSSAnalysisBetweenTime time1 time2 conn = do
  [n] <- liftIO $ (runQuery conn (countRSSAnalysisBetweenTime time1 time2) :: IO [Int64])
  return n

getRSSAnalysisAll :: Connection -> IO [An.RSSAnalysisH]
getRSSAnalysisAll conn = runQuery conn queryRSSAnalysisAll

getRSSAnalysisBySource :: Connection -> String -> IO [An.RSSAnalysisH]
getRSSAnalysisBySource conn src = runQuery conn (queryRSSAnalysisBySource src)

getRSSAnalysisByTime :: Connection -> UTCTime -> IO [An.RSSAnalysisH]
getRSSAnalysisByTime conn time = runQuery conn (queryRSSAnalysisByTime time)

getRSSAnalysisBySourceAndTime :: Connection -> String -> UTCTime -> IO [An.RSSAnalysisH]
getRSSAnalysisBySourceAndTime conn src time = runQuery conn (queryRSSAnalysisBySourceAndTime src time)

getRSSAnalysisByHash :: Connection -> ByteString -> IO [An.RSSAnalysisH]
getRSSAnalysisByHash conn hsh = (runQuery conn (queryRSSAnalysisByHash hsh) :: IO [An.RSSAnalysisH])
