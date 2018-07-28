{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module SemanticParserAPI.Compute.Type.Status where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (FromJSON,ToJSON)
import           Data.Binary         (Binary)
import           Data.HashMap.Strict (HashMap)
-- import           Data.HashSet (HashSet)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)


type Status = HashMap Text Bool -- on/off

data StatusQuery = SQ
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data StatusResult = SR [(Text,Bool)]
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

-- data ProcessStatus = PStat { _processStatus :: HashSet ProcessId }
