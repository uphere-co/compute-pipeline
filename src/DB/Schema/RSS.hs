{-# LANGUAGE DeriveGeneric #-}

module DB.Schema.RSS where

import           Database.Beam
import           GHC.Generics (Generic)
--
import           DB.Schema.RSS.Analysis     (RSSAnalysisT)
import           DB.Schema.RSS.Article      (RSSArticleT)
import           DB.Schema.RSS.ErrorArticle (RSSErrorArticleT)


data RSSDB f = RSSDB { _rssArticles :: f (TableEntity RSSArticleT)
                     , _rssAnalyses :: f (TableEntity RSSAnalysisT)
                     , _rssErrorArticles :: f (TableEntity RSSErrorArticleT)
                     }
             deriving Generic

instance Database RSSDB

rssDB :: DatabaseSettings be RSSDB
rssDB = defaultDbSettings

