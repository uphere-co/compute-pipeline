{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.RSS.Analysis where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro


data RSSAnalysisT f  = RSSAnalysis { _rssAnalysisId      :: Columnar f Int
                                   , _rssAnalysisHash    :: Columnar f ByteString
                                   , _rssAnalysisSource  :: Columnar f Text
                                   , _rssAnalysisCoreNLP :: Columnar f (Maybe Bool)
                                   , _rssAnalysisSRL     :: Columnar f (Maybe Bool)
                                   , _rssAnalysisNER     :: Columnar f (Maybe Bool)
                                   , _rssAnalysisCreated :: Columnar f UTCTime
                                   }
                     deriving Generic

instance Beamable RSSAnalysisT

instance Table RSSAnalysisT where
  data PrimaryKey RSSAnalysisT f = RSSAnalysisKey (Columnar f ByteString) deriving Generic
  primaryKey = RSSAnalysisKey <$> _rssAnalysisHash

instance Beamable (PrimaryKey RSSAnalysisT)

type RSSAnalysis = RSSAnalysisT Identity

deriving instance Show RSSAnalysis

RSSAnalysis (LensFor rssAnalysisId)
            (LensFor rssAnalysisHash)
            (LensFor rssAnalysisSource)
            (LensFor rssAnalysisCoreNLP)
            (LensFor rssAnalysisSRL)
            (LensFor rssAnalysisNER)
            (LensFor rssAnalysisCreated) = tableLenses

-- The PostgreSQL table was created as follows.

-- create table rssanalysis (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   corenlp boolean,
--   srl boolean,
--   ner boolean,
--   created timestamp with time zone NOT NULL,

--   constraint unique_rssanalysis_hash UNIQUE (hash)
-- );


-- from DB.Operation

uploadRSSAnalysis :: (ToRSSAnalysis a) => Connection -> a -> IO ()
uploadRSSAnalysis conn x = do
  let a = toRSSAnalysis x
  runInsert conn An.table $
    An.newRSSAnalysis (a ^. rss_analysis_hash) (a ^. rss_analysis_source) (a ^. rss_analysis_corenlp) (a ^. rss_analysis_srl) (a ^. rss_analysis_ner) (a ^. rss_analysis_created)
  return ()

updateRSSAnalysisStatus :: Connection -> ByteString -> (Maybe Bool,Maybe Bool,Maybe Bool) -> IO ()
updateRSSAnalysisStatus conn hsh (mb1,mb2,mb3) = do
  let f ov mb = if (isNothing mb) then (Just ov) else (if (fromJust mb) then ((toNullable . constant) <$> Just True) else ((toNullable . constant) <$> Nothing))
  runUpdate conn An.table (\(An.RSSAnalysis i hsh src mcore msrl mner ctm) -> An.RSSAnalysis (Just i) (Just hsh) (Just src) (f mcore mb1) (f msrl mb2) (f mner mb3) (Just ctm))
    (\x -> (An._hash x) .== (constant hsh))
  return ()

uploadRSSAnalysisIfMissing :: (ToRSSAnalysis a) => Connection -> a -> IO ()
uploadRSSAnalysisIfMissing conn x = do
  let a = toRSSAnalysis x
  as' <- getRSSAnalysisByHash conn (a ^. rss_analysis_hash)
  case as' of
    []  -> uploadRSSAnalysis conn a
    _as -> putStrLn "Already exists"

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
