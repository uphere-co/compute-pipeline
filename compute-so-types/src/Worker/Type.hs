{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Worker.Type where

import           Control.Concurrent.STM      ( TMVar )
import           Control.DeepSeq             ( NFData )
import           Control.Distributed.Process ( ProcessId )
import           Control.Distributed.Process.Internal.Types ( LocalProcessId, NodeId )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A       ( typeMismatch )
import qualified Data.ByteString.Base64 as B64
import           Data.Text                   ( Text )
import           Data.Text.Encoding          ( decodeUtf8, encodeUtf8 )
import           GHC.Generics                ( Generic )
import           Network.Transport           ( EndPointAddress(..) )
import           Network.Wai                 ( Application )
------


data NetworkConfig = NetworkConfig {
                       hostg :: Text
                     , hostl :: Text
                     , port :: Int
                     }
                   deriving (Generic,Show,Eq,Ord,NFData,A.FromJSON,A.ToJSON)

data CellConfig = CellConfig {
                    cellName :: Text
                  , cellAddress :: NetworkConfig
                  }
                deriving (Generic,Show,Eq,Ord,NFData,A.FromJSON,A.ToJSON)

data ComputeConfig = ComputeConfig {
                       computeServer :: NetworkConfig
                     , computeWeb :: NetworkConfig
                     , computeCells :: [CellConfig]
                     , computeBypassNER :: Bool
                     , computeBypassTEXTNER :: Bool
                     }
                   deriving (Generic,Show,Eq,Ord,NFData,A.FromJSON,A.ToJSON)

data ComputeWorkerOption =
  ComputeWorkerOption
  { servLangConfig :: FilePath
  , servComputeConfig :: FilePath
  }
  deriving (Show,Eq,Ord,Generic,NFData)


-- orphan instance

instance A.FromJSON EndPointAddress where
  parseJSON (A.String txt) = case B64.decode (encodeUtf8 txt) of
                               Left e   -> fail (show e)
                               Right bs -> pure (EndPointAddress bs)
  parseJSON invalid        = A.typeMismatch "EndPointAddress" invalid

instance A.ToJSON   EndPointAddress where
  toJSON (EndPointAddress bs) = A.String $ decodeUtf8 (B64.encode bs)

instance A.FromJSON NodeId
instance A.ToJSON   NodeId

instance A.FromJSON LocalProcessId
instance A.ToJSON   LocalProcessId

instance A.FromJSON ProcessId
instance A.ToJSON   ProcessId

data WorkerRole =
    Master
      Text       -- ^ name
{-      ComputeWorkerOption -- ^ options
      FilePath            -- ^ so file -}
  | Slave
      Text       -- ^ name
      CellConfig -- ^ master config
      ProcessId  -- ^ master process id
{-       Text                -- ^ name
      ComputeWorkerOption -- ^ options
      FilePath            -- ^ so file  -}
  deriving (Show,Eq,Ord,Generic,NFData,A.FromJSON,A.ToJSON)


-- | The set of functions that you want to expose from your shared object
data SOHandle = SOHandle
                { soApplication :: Application
                , soProcess :: TMVar ProcessId -> (WorkerRole,CellConfig) -> IO ()
                }
              deriving (Generic, NFData)