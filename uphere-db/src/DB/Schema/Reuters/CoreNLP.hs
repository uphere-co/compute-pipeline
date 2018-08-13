{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module DB.Schema.Reuters.CoreNLP where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Database.Beam         (Beamable,Columnar,Identity,LensFor(..),Table(..),tableLenses)
import GHC.Generics          (Generic)
import Lens.Micro            (Lens')

data CoreNLPT f = CoreNLP { _coreNLPHash    :: Columnar f ByteString
                          , _coreNLPResult  :: Columnar f (Maybe Text)
                          , _coreNLPCreated :: Columnar f UTCTime
                          }
                deriving Generic

instance Beamable CoreNLPT

instance Table CoreNLPT where
  data PrimaryKey CoreNLPT f = CoreNLPKey (Columnar f ByteString)
                             deriving Generic
  primaryKey = CoreNLPKey <$> _coreNLPHash

instance Beamable (PrimaryKey CoreNLPT)

type CoreNLP = CoreNLPT Identity

deriving instance Show CoreNLP

coreNLPHash    :: (Functor f) => Lens' (CoreNLPT f) (Columnar f ByteString)
coreNLPResult  :: (Functor f) => Lens' (CoreNLPT f) (Columnar f (Maybe Text))
coreNLPCreated :: (Functor f) => Lens' (CoreNLPT f) (Columnar f UTCTime)
CoreNLP (LensFor coreNLPHash)
        (LensFor coreNLPResult)
        (LensFor coreNLPCreated) = tableLenses

-- The PostgreSQL table was created as follows.
--
-- create table corenlp (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );
