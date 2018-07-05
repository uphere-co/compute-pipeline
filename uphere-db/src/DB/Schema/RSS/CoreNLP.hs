{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.RSS.CoreNLP where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

data AnalysisCoreNLPT f = AnalysisCoreNLP { _coreNLPHash    :: Columnar f ByteString
                                          , _coreNLPResult  :: Columnar f (Maybe Text)
                                          , _coreNLPCreated :: Columnar f UTCTime
                                          }
                        deriving Generic

instance Beamable AnalysisCoreNLPT

instance Table AnalysisCoreNLPT where
  data PrimaryKey AnalysisCoreNLPT f = AnalysisCoreNLPKey (Columnar f ByteString)
                                     deriving Generic
  primaryKey = AnalysisCoreNLPKey <$> _coreNLPHash

instance Beamable (PrimaryKey AnalysisCoreNLPT)

type AnalysisCoreNLP = AnalysisCoreNLPT Identity

deriving instance Show AnalysisCoreNLP

AnalysisCoreNLP (LensFor coreNLPHash)
                (LensFor coreNLPResult)
                (LensFor coreNLPCreated) = tableLenses


-- The PostgreSQL table was created as follows.
--
-- create table corenlp (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );
