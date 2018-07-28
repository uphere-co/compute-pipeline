{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module SemanticParserAPI.Compute.Type.Status where

import           Control.DeepSeq             (NFData)
import           Control.Distributed.Process (ProcessId)
import           Control.Lens                (makeLenses)
import           Data.Aeson                  (FromJSON,ToJSON)
import           Data.Binary                 (Binary)
import           Data.HashMap.Strict         (HashMap)
-- import           Data.HashSet                (HashSet)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)


data Status = Status { _statusNodes :: HashMap Text (Maybe ProcessId) -- Bool -- on/off node
                     -- , _statusLinkedProcesses :: HashSet ProcessId
                     }
              deriving (Show)
makeLenses ''Status

data StatusQuery = SQ
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data StatusResult = SR [(Text,Bool)]
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

