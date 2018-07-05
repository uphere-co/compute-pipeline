{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.RSS.SRL where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

data AnalysisSRLT f = AnalysisSRL { _srlHash    :: Columnar f ByteString
                                  , _srlResult  :: Columnar f (Maybe Text)
                                  , _srlCreated :: Columnar f UTCTime
                                  }
                    deriving Generic

instance Beamable AnalysisSRLT

instance Table AnalysisSRLT where
  data PrimaryKey AnalysisSRLT f = AnalysisSRLKey (Columnar f ByteString)
                                     deriving Generic
  primaryKey = AnalysisSRLKey <$> _srlHash

instance Beamable (PrimaryKey AnalysisSRLT)

type AnalysisSRL = AnalysisSRLT Identity

deriving instance Show AnalysisSRL

AnalysisSRL (LensFor srlHash)
            (LensFor srlResult)
            (LensFor srlCreated) = tableLenses


-- The PostgreSQL table was created as follows.
--
-- create table srl (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );

