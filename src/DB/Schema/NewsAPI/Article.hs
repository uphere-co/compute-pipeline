{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DB.Schema.NewsAPI.Article where

import           Data.ByteString.Char8
import           Data.Text
import           Data.Time.LocalTime
import           Data.Time.Clock
import           Database.Beam
import           Lens.Micro

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


-- from newsapi

{- 
uploadArticle :: PGS.Connection -> NewsAPIArticle -> IO ()
uploadArticle conn NewsAPIArticle {..} = do
  runInsert conn A.table $
    A.newArticle article_id article_source article_created
  return ()


queryArticleAll :: Query (To Column (A.Article))
queryArticleAll = proc () -> do
  r <- A.queryAll -< ()
  returnA -< r

queryArticleBySource :: String -> Query (To Column (A.Article))
queryArticleBySource src = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  returnA -< r

queryArticleByTime :: UTCTime -> Query (To Column (A.Article))
queryArticleByTime time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r

queryArticleBySourceAndTime :: String -> UTCTime -> Query (To Column (A.Article))
queryArticleBySourceAndTime src time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r





getArticleAll conn = (runQuery conn queryArticleAll :: IO [A.ArticleH])

getArticleBySource src conn = (runQuery conn (queryArticleBySource src) :: IO [A.ArticleH])

getArticleByTime time conn = (runQuery conn (queryArticleByTime time) :: IO [A.ArticleH])

getArticleBySourceAndTime conn src time = (runQuery conn (queryArticleBySourceAndTime src time) :: IO [A.ArticleH])
-}
