{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.RSS.NER where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

data AnalysisNERT f = AnalysisNER { _nerHash    :: Columnar f ByteString
                                  , _nerResult  :: Columnar f (Maybe Text)
                                  , _nerCreated :: Columnar f UTCTime
                                  }
                    deriving Generic

instance Beamable AnalysisNERT

instance Table AnalysisNERT where
  data PrimaryKey AnalysisNERT f = AnalysisNERKey (Columnar f ByteString)
                                     deriving Generic
  primaryKey = AnalysisNERKey <$> _nerHash

instance Beamable (PrimaryKey AnalysisNERT)

type AnalysisNER = AnalysisNERT Identity

deriving instance Show AnalysisNER

AnalysisNER (LensFor nerHash)
            (LensFor nerResult)
            (LensFor nerCreated) = tableLenses


-- The PostgreSQL table was created as follows.
--
-- create table ner (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );

