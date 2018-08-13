{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module DB.Schema.Reuters.NER where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Database.Beam         (Beamable,Columnar,Identity,LensFor(..),Table(..),tableLenses)
import GHC.Generics          (Generic)
import Lens.Micro            (Lens')

data NERT f = NER { _nerHash    :: Columnar f ByteString
                  , _nerResult  :: Columnar f (Maybe Text)
                  , _nerCreated :: Columnar f UTCTime
                  }
            deriving Generic

instance Beamable NERT

instance Table NERT where
  data PrimaryKey NERT f = NERKey (Columnar f ByteString)
                         deriving Generic
  primaryKey = NERKey <$> _nerHash

instance Beamable (PrimaryKey NERT)

type NER = NERT Identity

deriving instance Show NER

nerHash    :: (Functor f) => Lens' (NERT f) (Columnar f ByteString)
nerResult  :: (Functor f) => Lens' (NERT f) (Columnar f (Maybe Text))
nerCreated :: (Functor f) => Lens' (NERT f) (Columnar f UTCTime)
NER (LensFor nerHash)
    (LensFor nerResult)
    (LensFor nerCreated) = tableLenses

-- The PostgreSQL table was created as follows.
--
-- create table ner (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );

