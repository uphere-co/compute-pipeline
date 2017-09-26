{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.Article where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data Article = Article { _id :: Int
                           , _sha256 :: ByteString
                           , _source :: Text
                           , _created :: UTCTime
                           }
                 deriving Show |])
  
$(makeAdaptorAndInstance "pArticle" ''ArticleP)

$(makeTable "article" 'pArticle ''ArticleP)

queryAll :: Query (To Column Article)
queryAll = queryTable table 

-- smart constructor for inserting a new value.
newArticle :: ByteString
           -> Text
           -> UTCTime
           -> To Maybe (To Column Article)
newArticle s sn ct
  = Article Nothing (Just (constant s))
                    (Just (constant sn))
                    (Just (constant ct))

-- The PostgreSQL table was created as follows.

-- create table article (
--   id serial PRIMARY KEY,
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_sha256 UNIQUE (sha256)
-- );

-- MODIFIED
-- ALTER TABLE article ADD COLUMN content_hash char(64);
-- ALTER TABLE article ALTER COLUMN content_hash SET NOT NULL;
-- ALTER TABLE article ALTER COLUMN created SET NOT NULL;
-- ALTER TABLE article ALTER COLUMN content_hash SET NOT NULL;
-- ALTER TABLE article DROP COLUMN content_hash RESTRICT;
