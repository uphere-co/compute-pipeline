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
    data ErrorArticle = ErrorArticle { _hash     :: ByteString
                                     , _source   :: Text
                                     , _errormsg :: Text
                                     , _created  :: UTCTime
                                     }
                      deriving Show |])

$(makeAdaptorAndInstance "pErrorArticle" ''ErrorArticleP)

$(makeTable "errorrssarticle" 'pErrorArticle ''ErrorArticleP)

queryAll :: Query (To Column ErrorArticle)
queryAll = queryTable table 

-- smart constructor for inserting a new value.
newErrorArticle :: ByteString
                -> Text
                -> Text
                -> UTCTime
                -> To Maybe (To Column ErrorArticle)
newErrorArticle hsh src err ctm
  = ErrorArticle (Just (constant hsh))
                 (Just (constant src))
                 (Just (constant err))
                 (Just (constant ctm))

-- The PostgreSQL table was created as follows.

-- create table errorrssarticle (
--   hash bytea NOT NULL,
--   source text NOT NULL,
--   errormsg text NOT NULL,
--   created timestamp with time zone,
--   constraint unique_sha256_errorarticle UNIQUE (hash)
-- );
