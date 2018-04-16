{-# LANGUAGE DeriveGeneric #-}

module DB.Schema.NewsAPI where

import           Database.Beam
import           GHC.Generics (Generic)
--
import           DB.Schema.NewsAPI.Article      (ArticleT)
import           DB.Schema.NewsAPI.ArticleError (ArticleErrorT)
import           DB.Schema.NewsAPI.Analysis     (AnalysisT)


data NewsAPIDB f = ArticleDB { _newsapiArticles :: f (TableEntity ArticleT)
                             , _newsapiAnalyses :: f (TableEntity AnalysisT)
                             , _newsapiArticleErrors :: f (TableEntity ArticleErrorT)
                             }

                 deriving Generic

instance Database NewsAPIDB

newsAPIDB :: DatabaseSettings be NewsAPIDB
newsAPIDB = defaultDbSettings

