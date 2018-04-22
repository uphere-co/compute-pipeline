{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.RSS.Summary where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

data SummaryT f  = Summary { _summaryId          :: Columnar f Int
                           , _summaryHash        :: Columnar f ByteString
                           , _summaryLink        :: Columnar f Text
                           , _summaryTitle       :: Columnar f Text
                           , _summaryDescription :: Columnar f Text
                           , _summaryPubDate     :: Columnar f UTCTime
                           }
                 deriving Generic

instance Beamable SummaryT

instance Table SummaryT where
  data PrimaryKey SummaryT f = SummaryKey (Columnar f Int) deriving Generic
  primaryKey = SummaryKey <$> _summaryId

instance Beamable (PrimaryKey SummaryT)

type Summary = SummaryT Identity

deriving instance Show Summary


-- The PostgreSQL table was created as follows.

-- create table summary (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   link text NOT NULL,
--   title text NOT NULL,
--   description text NOT NULL,
--   pubDate timestamp with time zone NOT NULL
--
--   constraint unique_summary_hash UNIQUE (hash);
-- );
