{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DB.Schema.RSS where

import           Database.Beam
import           GHC.Generics (Generic)
--
import           DB.Schema.RSS.Analysis     (RSSAnalysisT)
import           DB.Schema.RSS.Article      (RSSArticleT)
import           DB.Schema.RSS.ErrorArticle (RSSErrorArticleT)
import           DB.Schema.RSS.Summary      (SummaryT)


data RSSDB f = RSSDB { _rssArticles :: f (TableEntity RSSArticleT)
                     , _rssAnalyses :: f (TableEntity RSSAnalysisT)
                     , _rssErrorArticles :: f (TableEntity RSSErrorArticleT)
                     , _summaries :: f (TableEntity SummaryT)
                     }
             deriving Generic

instance Database be RSSDB

rssDB :: DatabaseSettings be RSSDB
rssDB = defaultDbSettings

