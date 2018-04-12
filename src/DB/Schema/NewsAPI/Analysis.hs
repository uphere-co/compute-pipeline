{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NewsAPI.Analysis where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data Analysis = Analysis { _sha256   :: ByteString
                             , _source   :: Text
                             , _corenlp  :: Nullable Bool
                             , _srl      :: Nullable Bool
                             , _ner      :: Nullable Bool
                             , _created  :: UTCTime
                             }
                  deriving Show |])

$(makeAdaptorAndInstance "pAnalysis" ''AnalysisP)

$(makeTable "analysis" 'pAnalysis ''AnalysisP)

queryAll :: Query (To Column Analysis)
queryAll = queryTable DB.Schema.NewsAPI.Analysis.table

-- smart constructor for inserting a new value.
newAnalysis :: ByteString
           -> Text
           -> Maybe Bool
           -> Maybe Bool
           -> Maybe Bool
           -> UTCTime
           -> To Maybe (To Column Analysis)
newAnalysis s sr mcore msrl mner ct
  = Analysis (Just (constant s))
             (Just (constant sr))
             ((toNullable . constant) <$> mcore)
             ((toNullable . constant) <$> msrl)
             ((toNullable . constant) <$> mner)
             (Just (constant ct))

-- The PostgreSQL table was created as follows.

-- Obsolete Since 20170907
-- create table analysis (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   analysis text NOT NULL,
--   created timestamp with time zone NOT NULL,

--   constraint unique_sha256_analysis UNIQUE (sha256)
-- );

-- Use this after 20170907
-- create table analysis (
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   corenlp boolean,
--   srl boolean,
--   ner boolean,
--   created timestamp with time zone NOT NULL,

--   constraint unique_sha256_analysis UNIQUE (sha256)
-- );
