{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DB.Schema.RSS.ErrorArticle where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data RSSErrorArticle = RSSErrorArticle { _hash     :: ByteString
                                           , _source   :: Text
                                           , _errormsg :: Text
                                           , _created  :: UTCTime
                                           }
                         deriving Show |])

$(makeAdaptorAndInstance "pRSSErrorArticle" ''RSSErrorArticleP)

$(makeTable "rsserrorarticle" 'pRSSErrorArticle ''RSSErrorArticleP)

queryAll :: Query (To Column RSSErrorArticle)
queryAll = queryTable DB.Schema.RSS.ErrorArticle.table

-- smart constructor for inserting a new value.
newRSSErrorArticle :: ByteString
                   -> Text
                   -> Text
                   -> UTCTime
                   -> To Maybe (To Column RSSErrorArticle)
newRSSErrorArticle hsh src err ctm
  = RSSErrorArticle (Just (constant hsh))
                 (Just (constant src))
                 (Just (constant err))
                 (Just (constant ctm))

-- The PostgreSQL table was created as follows.

-- create table rsserrorarticle (
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   errormsg text NOT NULL,
--   created timestamp with time zone,
--   constraint unique_rsserrorarticle_hash UNIQUE (hash)
-- );
