{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.RSS.Analysis where

import Data.ByteString.Char8
import Data.Text
import Data.Time.LocalTime
import Data.Time.Clock
import Opaleye                    hiding (constant)
import Model.Opaleye.TH
import Model.Opaleye.ShowConstant (constant)
import Prelude

$(makeTypes [d|
    data RSSAnalysis = RSSAnalysis { _id       :: Int
                                   , _sha256   :: ByteString
                                   , _source   :: Text
                                   , _corenlp  :: Nullable Bool
                                   , _srl      :: Nullable Bool
                                   , _ner      :: Nullable Bool
                                   , _created  :: UTCTime
                                   }
                     deriving Show |])

$(makeAdaptorAndInstance "pRSSAnalysis" ''RSSAnalysisP)

$(makeTable "rssanalysis" 'pRSSAnalysis ''RSSAnalysisP)

queryAll :: Query (To Column RSSAnalysis)
queryAll = queryTable table

-- smart constructor for inserting a new value.                                                                               
newRSSAnalysis :: ByteString
               -> Text
               -> Maybe Bool
               -> Maybe Bool
               -> Maybe Bool
               -> UTCTime
               -> To Maybe (To Column RSSAnalysis)
newRSSAnalysis s sr mcore msrl mner ct
  = RSSAnalysis Nothing
                (Just (constant s))
                (Just (constant sr))
                ((toNullable . constant) <$> mcore)
                ((toNullable . constant) <$> msrl)
                ((toNullable . constant) <$> mner)
                (Just (constant ct))

-- The PostgreSQL table was created as follows.

-- create table rssanalysis (
--   id serial PRIMARY KEY,
--   sha256 bytea NOT NULL,
--   source text NOT NULL,
--   corenlp boolean,
--   srl boolean,
--   ner boolean,
--   created timestamp with time zone NOT NULL,

--   constraint unique_rssanalysis_sha256 UNIQUE (sha256)
-- );
