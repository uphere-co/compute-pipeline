{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.ArticleError where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro
-- import Opaleye                    hiding (constant)
-- import Model.Opaleye.TH
-- import Model.Opaleye.ShowConstant (constant)
import Prelude

data ArticleErrorT f = ArticleError { _aerrorSHA256 :: Columnar f ByteString
                                    , _aerrorSource :: Columnar f Text
                                    , _aerrroCreated :: Columnar f UTCTime
                                    }
                     deriving Generic

instance Beamable ArticleErrorT

instance Table ArticleErrorT where
  data PrimaryKey ArticleErrorT f = ArticleErrorKey (Columnar f ByteString) deriving Generic
  primaryKey = ArticleErrorKey <$> _aerrorSHA256

instance Beamable (PrimaryKey ArticleErrorT)

type ArticleError = ArticleErrorT Identity

deriving instance Show ArticleError

ArticleError (LensFor aerrorSHA256)
             (LensFor aerrorSource)
             (LensFor aerrorCreated) = tableLenses


{-
queryAll :: Query (To Column ArticleError)
queryAll = queryTable DB.Schema.NewsAPI.ArticleError.table

-- smart constructor for inserting a new value.
newArticleError :: ByteString
                -> Text
                -> UTCTime
                -> To Maybe (To Column ArticleError)
newArticleError s sn ct
  = ArticleError (Just (constant s))
                 (Just (constant sn))
                 (Just (constant ct))

-}

-- The PostgreSQL table was created as follows.

-- create table articleerror (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   created timestamp with time zone,

--   constraint unique_sha256_error UNIQUE (sha256)
-- );
