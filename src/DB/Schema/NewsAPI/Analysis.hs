{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.Analysis where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro


data AnalysisT f = Analysis { _analysisSHA256   :: Columnar f ByteString
                            , _analysisSource   :: Columnar f Text
                            , _analysisCoreNLP  :: Columnar f (Maybe Bool)
                            , _analysisSRL      :: Columnar f (Maybe Bool)
                            , _analysisNER      :: Columnar f (Maybe Bool)
                            , _analysisCreated  :: Columnar f UTCTime
                         }
              deriving Generic

instance Beamable AnalysisT

instance Table AnalysisT where
  data PrimaryKey AnalysisT f = AnalysisKey (Columnar f ByteString) deriving Generic
  primaryKey = AnalysisKey <$> _analysisSHA256

instance Beamable (PrimaryKey AnalysisT)

type Analysis = AnalysisT Identity

deriving instance Show Analysis

Analysis (LensFor analysisSHA256)  (LensFor analysisSource)
         (LensFor analysisCoreNLP) (LensFor analysisSRL)
         (LensFor analysisNER)     (LensFor analysisCreated) = tableLenses


-- The PostgreSQL table was created as follows.

-- Obsolete Since 20170907
-- create table analysis (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   analysis text NOT NULL,
--   created timestamp with time zone NOT NULL,

--   constraint unique_sha256_analysis UNIQUE (sha256)
-- );

-- Use this after 20170907
-- create table analysis (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   corenlp boolean,
--   srl boolean,
--   ner boolean,
--   created timestamp with time zone NOT NULL,

--   constraint unique_sha256_analysis UNIQUE (sha256)
-- );



-- from newsapi

uploadAnalysis :: PGS.Connection -> NewsAPIAnalysisDB -> IO ()
uploadAnalysis conn NewsAPIAnalysisDB {..} = do
  runInsert conn Analysis.table $
    Analysis.newAnalysis analysis_hash analysis_source analysis_corenlp analysis_srl analysis_ner analysis_created
  return ()

uploadAnalysisIfMissing :: PGS.Connection -> NewsAPIAnalysisDB -> IO ()
uploadAnalysisIfMissing conn NewsAPIAnalysisDB {..} = do
  as' <- getAnalysisByHash analysis_hash conn
  case as' of
    [] -> uploadAnalysis conn (NewsAPIAnalysisDB {..})
    as -> print "Already exists"

updateAnalysisStatus :: PGS.Connection -> ByteString -> (Maybe Bool,Maybe Bool,Maybe Bool) -> IO ()
updateAnalysisStatus conn hsh (mb1,mb2,mb3) = do
  let f ov mb = if (isNothing mb) then (Just ov) else (if (fromJust mb) then ((toNullable . constant) <$> Just True) else ((toNullable . constant) <$> Nothing)) -- What happens if ov is null? I couldn't find a way to check if it is null.
                                                                                                                                                                -- Due to the test, if ov is null, then null is inserted despite outer Just.
  runUpdate conn Analysis.table (\(Analysis.Analysis s src mcore msrl mner ct) -> Analysis.Analysis (Just s) (Just src) (f mcore mb1) (f msrl mb2) (f mner mb3) (Just ct))
    (\x -> (Analysis._sha256 x) .== (constant hsh))
  return ()

queryAnalysisAll :: Query (To Column (Analysis.Analysis))
queryAnalysisAll = proc () -> do
  r <- Analysis.queryAll -< ()
  returnA -< r

queryAnalysisBySource :: String -> Query (To Column (Analysis.Analysis))
queryAnalysisBySource src = proc () -> do
  r <- Analysis.queryAll -< ()
  restrict -< Analysis._source r .== (constant (T.pack src))
  returnA -< r

queryAnalysisByTime :: UTCTime -> Query (To Column (Analysis.Analysis))
queryAnalysisByTime time = proc () -> do
  r <- Analysis.queryAll -< ()
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ Analysis._created r)
  returnA -< r

queryAnalysisBySourceAndTime :: String -> UTCTime -> Query (To Column (Analysis.Analysis))
queryAnalysisBySourceAndTime src time = proc () -> do
  r <- Analysis.queryAll -< ()
  restrict -< Analysis._source r .== (constant (T.pack src))
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ Analysis._created r)
  returnA -< r

queryAnalysisByHash :: ByteString -> Query (To Column (Analysis.Analysis))
queryAnalysisByHash hsh = proc () -> do
  r <- Analysis.queryAll -< ()
  restrict -< Analysis._sha256 r .== (constant hsh)
  returnA -< r

getAnalysisAll conn = (runQuery conn queryAnalysisAll :: IO [Analysis.AnalysisH])

getAnalysisBySource src conn = (runQuery conn (queryAnalysisBySource src) :: IO [Analysis.AnalysisH])

getAnalysisByTime time conn = (runQuery conn (queryAnalysisByTime time) :: IO [Analysis.AnalysisH])

getAnalysisBySourceAndTime conn src time = (runQuery conn (queryAnalysisBySourceAndTime src time) :: IO [Analysis.AnalysisH])

getAnalysisByHash hsh conn = (runQuery conn (queryAnalysisByHash hsh) :: IO [Analysis.AnalysisH])
