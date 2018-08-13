{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module DB.Schema.Reuters.CleanUp where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Database.Beam         (Beamable,Columnar,Identity,LensFor(..),Table(..),tableLenses)
import GHC.Generics          (Generic)
import Lens.Micro            (Lens')

data CleanUpT f = CleanUp { _cleanupHash        :: Columnar f ByteString
                          , _cleanupTitle       :: Columnar f Text
                          , _cleanupDescription :: Columnar f Text
                          , _cleanupCreated     :: Columnar f UTCTime
                          }
                deriving Generic

instance Beamable CleanUpT

instance Table CleanUpT where
  data PrimaryKey CleanUpT f = CleanUpKey (Columnar f ByteString)
                             deriving Generic
  primaryKey = CleanUpKey <$> _cleanupHash

instance Beamable (PrimaryKey CleanUpT)

type CleanUp = CleanUpT Identity

deriving instance Show CleanUp

cleanupHash        :: (Functor f) => Lens' (CleanUpT f) (Columnar f ByteString)
cleanupTitle       :: (Functor f) => Lens' (CleanUpT f) (Columnar f Text)
cleanupDescription :: (Functor f) => Lens' (CleanUpT f) (Columnar f Text)
cleanupCreated     :: (Functor f) => Lens' (CleanUpT f) (Columnar f UTCTime)
CleanUp (LensFor cleanupHash)
        (LensFor cleanupTitle)
        (LensFor cleanupDescription)  
        (LensFor cleanupCreated) = tableLenses

-- The PostgreSQL table was created as follows.
--
-- create table cleanup (
--   hash bytea PRIMARY KEY NOT NULL,
--   title text,
--   description text,
--   created timestamp with time zone NOT NULL
-- );
