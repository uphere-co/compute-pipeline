{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Type where

import Data.ByteString.Char8 (ByteString)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)

class UpHereDB a where
  upload  :: a -> IO ()
  upload2 :: a -> IO ()
  update  :: a -> IO ()

data ArticleDB = ArticleDB
  { _id      :: Int
  , _hash    :: ByteString
  , _source  :: Text
  , _created :: UTCTime
  } deriving (Show)

instance (Article a) => UpHereDB a where
  upload atl = do
    let article = toArticle atl
    return ()
    
{-
data AnalysisDB = AnalysisDB
  {
  }
data ArticleErrorDB = ArticleErrorDB { }
-}

class Article a where
  toArticle :: a -> ArticleDB


{-
class Analysis a where
  toAnalysis :: a -> AnalysisDB

class ArticleError a where
  toArticleError :: a -> ArticleErrorDB
-}
