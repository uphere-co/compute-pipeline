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
