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


{-

updateAnalysisStatus :: PGS.Connection -> ByteString -> (Maybe Bool,Maybe Bool,Maybe Bool) -> IO ()
updateAnalysisStatus conn hsh (mb1,mb2,mb3) = do
  let f ov mb = if (isNothing mb) then (Just ov) else (if (fromJust mb) then ((toNullable . constant) <$> Just True) else ((toNullable . constant) <$> Nothing)) -- What happens if ov is null? I couldn't find a way to check if it is null.
                                                                                                                                                                -- Due to the test, if ov is null, then null is inserted despite outer Just.
  runUpdate conn Analysis.table (\(Analysis.Analysis s src mcore msrl mner ct) -> Analysis.Analysis (Just s) (Just src) (f mcore mb1) (f msrl mb2) (f mner mb3) (Just ct))
    (\x -> (Analysis._sha256 x) .== (constant hsh))
  return ()



getAnalysisAll conn = (runQuery conn queryAnalysisAll :: IO [Analysis.AnalysisH])

getAnalysisBySource src conn = (runQuery conn (queryAnalysisBySource src) :: IO [Analysis.AnalysisH])

getAnalysisByTime time conn = (runQuery conn (queryAnalysisByTime time) :: IO [Analysis.AnalysisH])

getAnalysisBySourceAndTime conn src time = (runQuery conn (queryAnalysisBySourceAndTime src time) :: IO [Analysis.AnalysisH])

-}
