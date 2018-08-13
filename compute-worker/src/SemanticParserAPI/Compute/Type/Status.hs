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
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
--
import CloudHaskell.Util (RequestDuplex)
import Task.CoreNLP (QCoreNLP,RCoreNLP)

data NodeStatus = NodeStatus {
                    _nodeStatusMainProcessId :: ProcessId
                  , _nodeStatusIsServing :: Bool
                  , _nodeStatusNumServed :: Int
                  , _nodeStatusDuplex :: RequestDuplex QCoreNLP RCoreNLP -- TODO: Generalize this!
                  }

makeLenses ''NodeStatus

data Status = Status {
                _statusNodes :: HashMap Text (Maybe NodeStatus)
              }

makeLenses ''Status

data StatusQuery = SQ
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data StatusResult = SR [(Text,(Maybe (Bool,Int)))]
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

