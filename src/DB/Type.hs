{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Type where

import           Data.ByteString.Char8             (ByteString)
import           Data.Text                         (Text)
import           Data.Time.Clock                   (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS
--


data ArticleDB = ArticleDB
  { _id      :: Int
  , _hash    :: ByteString
  , _source  :: Text
  , _created :: UTCTime
  } deriving (Show)

{-
data AnalysisDB = AnalysisDB
  {
  }
data ArticleErrorDB = ArticleErrorDB { }
-}

class ToArticle a where
  toArticle :: a -> ArticleDB


{-
class Analysis a where
  toAnalysis :: a -> AnalysisDB

class ArticleError a where
  toArticleError :: a -> ArticleErrorDB
-}
