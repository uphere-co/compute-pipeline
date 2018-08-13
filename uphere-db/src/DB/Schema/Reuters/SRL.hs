{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.Reuters.SRL where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Database.Beam         (Beamable,Columnar,Identity,LensFor(..),Table(..),tableLenses)
import GHC.Generics          (Generic)
import Lens.Micro            (Lens')

data SRLT f = SRL { _srlHash    :: Columnar f ByteString
                  , _srlResult  :: Columnar f (Maybe Text)
                  , _srlCreated :: Columnar f UTCTime
                  }
            deriving Generic

instance Beamable SRLT

instance Table SRLT where
  data PrimaryKey SRLT f = SRLKey (Columnar f ByteString)
                                     deriving Generic
  primaryKey = SRLKey <$> _srlHash

instance Beamable (PrimaryKey SRLT)

type SRL = SRLT Identity

deriving instance Show SRL

srlHash    :: (Functor f) => Lens' (SRLT f) (Columnar f ByteString)
srlResult  :: (Functor f) => Lens' (SRLT f) (Columnar f (Maybe Text))
srlCreated :: (Functor f) => Lens' (SRLT f) (Columnar f UTCTime)
SRL (LensFor srlHash)
    (LensFor srlResult)
    (LensFor srlCreated) = tableLenses


-- The PostgreSQL table was created as follows.
--
-- create table srl (
--   hash bytea PRIMARY KEY NOT NULL,
--   result text,
--   created timestamp with time zone NOT NULL
-- );

