{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DB.Schema.RSS.Article where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data RSSArticle = RSSArticle { _id      :: Int
                                 , _hash    :: ByteString
                                 , _source  :: Text
                                 , _created :: UTCTime
                                 }
                    deriving Show |])
  
$(makeAdaptorAndInstance "pRSSArticle" ''RSSArticleP)

$(makeTable "rssarticle" 'pRSSArticle ''RSSArticleP)

queryAll :: Query (To Column RSSArticle)
queryAll = queryTable table 

-- smart constructor for inserting a new value.
newRSSArticle :: ByteString
              -> Text
              -> UTCTime
              -> To Maybe (To Column RSSArticle)
newRSSArticle hsh src ctm
  = RSSArticle Nothing (Just (constant hsh))
                       (Just (constant src))
                       (Just (constant ctm))

-- The PostgreSQL table was created as follows.

-- create table rssarticle (
--   id serial PRIMARY KEY,
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_rssarticle_hash UNIQUE (hash)
-- );
