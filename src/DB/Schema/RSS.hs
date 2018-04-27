{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module DB.Schema.RSS where

import           Database.Beam
import           GHC.Generics (Generic)
--
import           DB.Schema.RSS.Analysis     (RSSAnalysisT(..))
import           DB.Schema.RSS.Article      (RSSArticleT(..))
import           DB.Schema.RSS.CoreNLP      (AnalysisCoreNLPT(..))
import           DB.Schema.RSS.ErrorArticle (RSSErrorArticleT(..))
import           DB.Schema.RSS.Summary      (SummaryT(..))


data RSSDB f = RSSDB { _rssArticles      :: f (TableEntity RSSArticleT)
                     , _rssAnalyses      :: f (TableEntity RSSAnalysisT)
                     , _rssErrorArticles :: f (TableEntity RSSErrorArticleT)
                     , _summaries        :: f (TableEntity SummaryT)
                     , _coreNLPs         :: f (TableEntity AnalysisCoreNLPT)
                     }
             deriving Generic

instance Database be RSSDB

rssDB :: DatabaseSettings be RSSDB
rssDB = defaultDbSettings `withDbModification`
          dbModification
          { _rssArticles =
              modifyTable (\_ -> "rssarticle") $
                tableModification
                { _rssArticleId      = fieldNamed "id"
                , _rssArticleHash    = fieldNamed "hash"
                , _rssArticleSource  = fieldNamed "source"
                , _rssArticleCreated = fieldNamed "created"
                }
          , _rssAnalyses =
              modifyTable (\_ -> "rssanalysis") $
                tableModification
                { _rssAnalysisId      = fieldNamed "id"
                , _rssAnalysisHash    = fieldNamed "hash"
                , _rssAnalysisSource  = fieldNamed "source"
                , _rssAnalysisCoreNLP = fieldNamed "corenlp"
                , _rssAnalysisSRL     = fieldNamed "srl"
                , _rssAnalysisNER     = fieldNamed "ner"
                , _rssAnalysisCreated = fieldNamed "created"
                }
          , _rssErrorArticles =
              modifyTable (\_ -> "rsserrorarticle") $
                tableModification
                { _rssErrorHash    = fieldNamed "hash"
                , _rssErrorSource  = fieldNamed "source"
                , _rssErrorMsg     = fieldNamed "errormsg"
                , _rssErrorCreated = fieldNamed "created"
                }
          , _summaries =
              modifyTable (\_ -> "summary") $
                tableModification
                { _summaryId          = fieldNamed "id"
                , _summaryHash        = fieldNamed "hash"
                , _summaryLink        = fieldNamed "link"
                , _summaryTitle       = fieldNamed "title"
                , _summaryDescription = fieldNamed "description"
                , _summaryPubDate     = fieldNamed "pubdate"
                }
          , _coreNLPs =
              modifyTable (\_ -> "corenlp") $
                tableModification
                { _coreNLPHash    = fieldNamed "hash"
                , _coreNLPResult  = fieldNamed "result"
                , _coreNLPCreated = fieldNamed "created"
                }

          }
