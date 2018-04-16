{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.Article where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
-- import           Database.Beam.Postgres (Pg)
import           Lens.Micro
-- import Opaleye                    hiding (constant)
-- import Model.Opaleye.TH
-- import Model.Opaleye.ShowConstant (constant)
-- import Prelude

data ArticleT f = Article { _articleId      :: Columnar f Text
                          , _articleHash    :: Columnar f ByteString
                          , _articleSource  :: Columnar f Text
                          , _articleCreated :: Columnar f UTCTime
                          }
             deriving (Generic)

instance Beamable ArticleT

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleKey (Columnar f Text) deriving Generic
  primaryKey = ArticleKey <$> _articleId

instance Beamable (PrimaryKey ArticleT)

type Article = ArticleT Identity

deriving instance Show Article


Article (LensFor articleId) (LensFor articleHash)
        (LensFor articleSource) (LensFor articleCreated) = tableLenses


{- 
$(makeAdaptorAndInstance "pArticle" ''ArticleP)

$(makeTable "article" 'pArticle ''ArticleP)

queryAll :: Query (To Column Article)
queryAll = queryTable DB.Schema.NewsAPI.Article.table 

-- smart constructor for inserting a new value.
newArticle :: ByteString
           -> Text
           -> UTCTime
           -> To Maybe (To Column Article)
newArticle hsh src ctm
  = Article Nothing (Just (constant hsh))
                    (Just (constant src))
                    (Just (constant ctm))

-}

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
