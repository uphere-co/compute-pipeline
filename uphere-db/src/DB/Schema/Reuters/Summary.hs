{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.Reuters.Summary where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Database.Beam         (Beamable,Columnar,Identity,LensFor(..),Table(..),tableLenses)
import GHC.Generics          (Generic)
import Lens.Micro            (Lens')

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

summaryId          :: (Functor f) => Lens' (SummaryT f) (Columnar f Int)
summaryHash        :: (Functor f) => Lens' (SummaryT f) (Columnar f ByteString)
summaryLink        :: (Functor f) => Lens' (SummaryT f) (Columnar f Text)
summaryTitle       :: (Functor f) => Lens' (SummaryT f) (Columnar f Text)
summaryDescription :: (Functor f) => Lens' (SummaryT f) (Columnar f Text)
summaryPubDate     :: (Functor f) => Lens' (SummaryT f) (Columnar f UTCTime)
Summary (LensFor summaryId)
        (LensFor summaryHash)
        (LensFor summaryLink)
        (LensFor summaryTitle)
        (LensFor summaryDescription)
        (LensFor summaryPubDate)     = tableLenses


-- The PostgreSQL table was created as follows.

-- create table summary (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   link text NOT NULL,
--   title text NOT NULL,
--   description text NOT NULL,
--   pubdate timestamp with time zone NOT NULL
--
--   constraint unique_summary_hash UNIQUE (hash);
-- );
