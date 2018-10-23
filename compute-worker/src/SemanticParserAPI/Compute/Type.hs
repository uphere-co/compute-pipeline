{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module SemanticParserAPI.Compute.Type where

import           Data.Aeson                     (FromJSON,ToJSON)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
--

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

