{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker.Type where

import Control.DeepSeq         ( NFData )
import Data.Aeson (FromJSON,ToJSON)
import Data.Text (Text)
import GHC.Generics            ( Generic )
import Network.Wai             ( Application )
------



data NetworkConfig = NetworkConfig {
                       hostg :: Text
                     , hostl :: Text
                     , port :: Int
                     }
                   deriving (Generic,Show,FromJSON,ToJSON)

data CellConfig = CellConfig {
                    cellName :: Text
                  , cellAddress :: NetworkConfig
                  }
                deriving (Generic,Show,FromJSON,ToJSON)

data ComputeConfig = ComputeConfig {
                       computeServer :: NetworkConfig
                     , computeWeb :: NetworkConfig
                     , computeCells :: [CellConfig]
                     , computeBypassNER :: Bool
                     , computeBypassTEXTNER :: Bool
                     }
                   deriving (Generic,Show,FromJSON,ToJSON)

data ComputeWorkerOption =
  ComputeWorkerOption
  { servLangConfig :: FilePath
  , servComputeConfig :: FilePath
  }
  deriving (Show,Eq,Ord,Generic,NFData)


data WorkerRole =
    Master
{-      ComputeWorkerOption -- ^ options
      FilePath            -- ^ so file -}
  | Slave
{-      Text                -- ^ name
      ComputeWorkerOption -- ^ options
      FilePath            -- ^ so file -}
  deriving (Show,Eq,Ord,Generic,NFData,FromJSON,ToJSON)


-- | The set of functions that you want to expose from your shared object
data SOHandle = SOHandle
                { soApplication :: Application
                }
              deriving (Generic, NFData)
