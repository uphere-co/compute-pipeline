{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}


module DB.Type where

import           Control.Lens                      (makeLenses)
import           Data.ByteString.Char8             (ByteString)
import           Data.Text                         (Text)
import           Data.Time.Clock                   (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS
--

data RSSArticleDB = RSSArticleDB
  { _rss_article_hash    :: ByteString
  , _rss_article_source  :: Text
  , _rss_article_created :: UTCTime
  } deriving (Show)

makeLenses ''RSSArticleDB

data RSSErrorArticleDB = RSSErrorArticleDB
  { _rss_error_article_hash :: ByteString
  , _rss_error_article_source :: Text
  , _rss_error_article_errormsg :: Text
  , _rss_error_article_created :: UTCTime
  } deriving (Show)

makeLenses ''RSSErrorArticleDB

data RSSAnalysisDB = RSSAnalysisDB
  { _rss_analysis_hash    :: ByteString
  , _rss_analysis_source  :: Text
  , _rss_analysis_corenlp :: Maybe Bool
  , _rss_analysis_srl     :: Maybe Bool
  , _rss_analysis_ner     :: Maybe Bool
  , _rss_analysis_created :: UTCTime
  } deriving (Show)

makeLenses ''RSSAnalysisDB

class ToRSSArticle a where
  toRSSArticle :: a -> RSSArticleDB

instance ToRSSArticle RSSArticleDB where
  toRSSArticle = id

class ToRSSAnalysis a where
  toRSSAnalysis :: a -> RSSAnalysisDB

instance ToRSSAnalysis RSSAnalysisDB where
  toRSSAnalysis = id

class ToRSSErrorArticle a where
  toRSSErrorArticle :: a -> RSSErrorArticleDB

instance ToRSSErrorArticle RSSErrorArticleDB where
  toRSSErrorArticle = id
