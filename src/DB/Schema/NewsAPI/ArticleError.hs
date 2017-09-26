{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.ArticleError where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data ArticleError = ArticleError { _sha256 :: ByteString
                                     , _source :: Text
                                     , _created :: UTCTime
                                     }
                 deriving Show |])

$(makeAdaptorAndInstance "pArticleError" ''ArticleErrorP)

$(makeTable "articleerror" 'pArticleError ''ArticleErrorP)

queryAll :: Query (To Column ArticleError)
queryAll = queryTable table 

-- smart constructor for inserting a new value.
newArticleError :: ByteString
                -> Text
                -> UTCTime
                -> To Maybe (To Column ArticleError)
newArticleError s sn ct
  = ArticleError (Just (constant s))
                 (Just (constant sn))
                 (Just (constant ct))

-- The PostgreSQL table was created as follows.

-- create table articleerror (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_sha256_error UNIQUE (sha256)
-- );
