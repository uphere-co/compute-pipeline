{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module DB.Schema.Reuters where

import Database.Beam
import GHC.Generics (Generic)
--
import DB.Schema.Reuters.CleanUp (CleanUpT(..))
import DB.Schema.Reuters.CoreNLP (CoreNLPT(..))
import DB.Schema.Reuters.NER     (NERT(..))
import DB.Schema.Reuters.SRL     (SRLT(..))
import DB.Schema.Reuters.Summary (SummaryT(..))


data ReutersDB f = ReutersDB {
                   _summaries        :: f (TableEntity SummaryT)
                   , _cleanups         :: f (TableEntity CleanUpT)
                   , _coreNLPs         :: f (TableEntity CoreNLPT)
                   , _SRLs             :: f (TableEntity SRLT)
                   , _NERs             :: f (TableEntity NERT)
                   }
                 deriving Generic

instance Database be ReutersDB

reutersDB :: DatabaseSettings be ReutersDB
reutersDB = defaultDbSettings `withDbModification`
              dbModification
              { _summaries =
                  modifyTable (\_ -> "summary") $
                    tableModification
                    { _summaryId          = fieldNamed "id"
                    , _summaryHash        = fieldNamed "hash"
                    , _summaryLink        = fieldNamed "link"
                    , _summaryTitle       = fieldNamed "title"
                    , _summaryDescription = fieldNamed "description"
                    , _summaryPubDate     = fieldNamed "pubdate"
                    }
              , _cleanups =
                  modifyTable (\_ -> "cleanup") $
                    tableModification
                    { _cleanupHash        = fieldNamed "hash"
                    , _cleanupTitle       = fieldNamed "link"
                    , _cleanupDescription = fieldNamed "title"
                    , _cleanupCreated     = fieldNamed "created"
                    }
              , _coreNLPs =
                  modifyTable (\_ -> "corenlp") $
                    tableModification
                    { _coreNLPHash    = fieldNamed "hash"
                    , _coreNLPResult  = fieldNamed "result"
                    , _coreNLPCreated = fieldNamed "created"
                    }
              , _SRLs =
                  modifyTable (\_ -> "srl") $
                    tableModification
                    { _srlHash    = fieldNamed "hash"
                    , _srlResult  = fieldNamed "result"
                    , _srlCreated = fieldNamed "created"
                    }
              , _NERs =
                  modifyTable (\_ -> "ner") $
                    tableModification
                    { _nerHash    = fieldNamed "hash"
                    , _nerResult  = fieldNamed "result"
                    , _nerCreated = fieldNamed "created"
                    }

              }
