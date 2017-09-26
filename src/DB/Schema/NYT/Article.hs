{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NYT.Article where

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
                           , _link :: Text
                           , _ptime :: UTCTime
                           }
                 deriving Show |])

$(makeAdaptorAndInstance "pArticle" ''ArticleP)

$(makeTable "nyt_article" 'pArticle ''ArticleP)

queryAll :: Query (To Column Article)
queryAll = queryTable table

-- smart constructor for inserting a new value.                                                                               
newArticle :: ByteString
           -> Text
           -> UTCTime
           -> To Maybe (To Column Article)
newArticle s l ct
  = Article Nothing (Just (constant s))
                    (Just (constant l))
                    (Just (constant ct))


-- The PostgreSQL table was created as follows.                                                                               

-- create table nyt_article (
--   id serial PRIMARY KEY,
--   sha256 bytea NOT NULL,
--   link text NOT NULL,
--   ptime timestamp with time zone NOT NULL,

--   constraint unique_sha256_nyt UNIQUE (sha256)
-- );
