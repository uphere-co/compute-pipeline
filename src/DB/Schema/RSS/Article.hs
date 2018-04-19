{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DB.Schema.RSS.Article where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro
-- import Opaleye                    hiding (constant)
-- import Model.Opaleye.TH
-- import Model.Opaleye.ShowConstant (constant)
-- import Prelude

data RSSArticleT f = RSSArticle { _rssArticleId      :: Columnar f Int
                                , _rssArticleHash    :: Columnar f ByteString
                                , _rssArticleSource  :: Columnar f Text
                                , _rssArticleCreated :: Columnar f UTCTime
                                }
                   deriving Generic

instance Beamable RSSArticleT

instance Table RSSArticleT where
  data PrimaryKey RSSArticleT f = RSSArticleKey (Columnar f Int) deriving Generic
  primaryKey = RSSArticleKey <$> _rssArticleId

instance Beamable (PrimaryKey RSSArticleT)

type RSSArticle = RSSArticleT Identity

RSSArticle (LensFor rssArticleId)
           (LensFor rssArticleHash)
           (LensFor rssArticleSource)
           (LensFor rssArticleCreated) = tableLenses

-- The PostgreSQL table was created as follows.

-- create table rssarticle (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_rssarticle_hash UNIQUE (hash)
-- );


-- from DB.Operation

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

queryRSSArticleBySourceAndBetTime :: String -> UTCTime -> UTCTime -> Query (To Column (A.RSSArticle))
queryRSSArticleBySourceAndBetTime src time1 time2 = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  restrict -< ((pgUTCTime time1 .<= (safeCoerceToRep $ A._created r)) .&& ((safeCoerceToRep $ A._created r) .<= pgUTCTime time2))
  returnA -< r

queryRSSArticleByHash :: ByteString -> Query (To Column (A.RSSArticle))
queryRSSArticleByHash hsh = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._hash r .== (constant hsh)
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
