{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DB.Schema.RSS.Article where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro
-- import Opaleye                    hiding (constant)
-- import Model.Opaleye.TH
-- import Model.Opaleye.ShowConstant (constant)
-- import Prelude

data RSSArticleT f = RSSArticle { _rssArticleId      :: Columnar f Int
                                , _rssArticleHash    :: Columnar f ByteString
                                , _rssArticleSource  :: Columnar f Text
                                , _rssArticleCreated :: Columnar f UTCTime
                                }
                   deriving Generic
  
instance Beamable RSSArticleT

instance Table RSSArticleT where
  data PrimaryKey RSSArticleT f = RSSArticleKey (Columnar f Int) deriving Generic
  primaryKey = RSSArticleKey <$> _rssArticleId

instance Beamable (PrimaryKey RSSArticleT)

type RSSArticle = RSSArticleT Identity

RSSArticle (LensFor rssArticleId)
           (LensFor rssArticleHash)
           (LensFor rssArticleSource)
           (LensFor rssArticleCreated) = tableLenses

-- The PostgreSQL table was created as follows.

-- create table rssarticle (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_rssarticle_hash UNIQUE (hash)
-- );
