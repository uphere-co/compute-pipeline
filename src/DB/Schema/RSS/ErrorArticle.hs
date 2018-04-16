{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DB.Schema.RSS.ErrorArticle where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

data RSSErrorArticleT f = RSSErrorArticle { _rssErrorHash     :: Columnar f ByteString
                                          , _rssErrorSource   :: Columnar f Text
                                          , _rssErrorMsg      :: Columnar f Text
                                          , _rssErrorCreated  :: Columnar f UTCTime
                                          }
                        deriving Generic

instance Beamable RSSErrorArticleT

instance Table RSSErrorArticleT where
  data PrimaryKey RSSErrorArticleT f = RSSErrorArticleKey (Columnar f ByteString)
                                     deriving Generic
  primaryKey = RSSErrorArticleKey <$> _rssErrorHash

instance Beamable (PrimaryKey RSSErrorArticleT)

type RSSErrorArticle = RSSErrorArticleT Identity

RSSErrorArticle (LensFor rssErrorHash)
                (LensFor rssErrorSource)
                (LensFor rssErrorMsg)
                (LensFor rssErrorCreated) = tableLenses

-- The PostgreSQL table was created as follows.

-- create table rsserrorarticle (
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   errormsg text NOT NULL,
--   created timestamp with time zone,
--   constraint unique_rsserrorarticle_hash UNIQUE (hash)
-- );
