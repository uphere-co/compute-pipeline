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


