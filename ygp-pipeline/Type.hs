{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Type where

import qualified Data.ByteString.Lazy as BL
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

data ConfigYGP = ConfigYGP
  { _engine_type             :: String
  , _corenlp_client_script   :: String
  , _word_uids_dump          :: String
  , _pos_uids_dump           :: String
  , _arclabel_uids_dump      :: String
  , _column_uids_dump        :: String
  , _country_uids_dump       :: String
  , _word_prob_dump          :: String
  , _corenlp_dumps           :: String
  , _dep_parsed_store        :: String
  , _dep_parsed_prefix       :: String
  , _wordvec_store           :: String
  , _voca_name               :: String
  , _w2vmodel_name           :: String
  , _w2v_float_t             :: String
  , _wikidata_entities       :: String
  , _wikidata_uids           :: String
  , _wikidata_properties     :: String
  , _named_entity_uids       :: String
  } deriving (Show, Generic)

data ConfigRSS = ConfigRSS
  { _engine_type             :: String
  , _corenlp_client_script   :: String
  , _word_uids_dump          :: String
  , _pos_uids_dump           :: String
  , _arclabel_uids_dump      :: String
  , _column_uids_dump        :: String
  , _word_prob_dump          :: String
  , _row_hashes              :: String
  , _corenlp_dumps           :: String
  , _dep_parsed_store        :: String
  , _dep_parsed_prefix       :: String
  , _wordvec_store           :: String
  , _voca_name               :: String
  , _w2vmodel_name           :: String
  , _w2v_float_t             :: String
  , _wikidata_entities       :: String
  , _wikidata_uids           :: String
  , _wikidata_properties     :: String
  , _named_entity_uids       :: String
  } deriving (Show, Generic)


instance ToJSON ConfigYGP where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ConfigYGP where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ConfigRSS where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ConfigRSS where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


getDefYGP :: IO ConfigYGP
getDefYGP = do
  ygp <- BL.readFile "../config.ygp.json.default"
  let mv =  decode ygp
  case mv of
    Nothing -> error "JSON is not valid."
    Just  v -> return v

getDefRSS :: IO ConfigRSS
getDefRSS = do
  rss <- BL.readFile "../config.rss.json.default"
  let mv =  decode rss
  case mv of
    Nothing -> error "JSON is not valid."
    Just  v -> return v


data EngineType = YGP | RSS

type MinorVersion = Int
