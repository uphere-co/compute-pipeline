{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Schema.NYT.Analysis where

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
                             , _analysis :: Text
                             , _created  :: UTCTime
                             }
                  deriving Show |])

$(makeAdaptorAndInstance "pAnalysis" ''AnalysisP)

$(makeTable "nyt_analysis" 'pAnalysis ''AnalysisP)

queryAll :: Query (To Column Analysis)
queryAll = queryTable table

-- smart constructor for inserting a new value.                                                                               
newAnalysis :: ByteString
           -> Text
           -> UTCTime
           -> To Maybe (To Column Analysis)
newAnalysis s a ct
  = Analysis (Just (constant s))
             (Just (constant a))
             (Just (constant ct))


-- The PostgreSQL table was created as follows.

-- create table nyt_analysis (
--   sha256 bytea NOT NULL,
--   analysis text NOT NULL,
--   created timestamp with time zone NOT NULL,

--   constraint unique_sha256_nyt_analysis UNIQUE (sha256)
-- );
