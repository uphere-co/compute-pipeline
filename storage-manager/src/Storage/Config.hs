{-# LANGUAGE DeriveGeneric #-}
module Storage.Config where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AWSCredential = AWSCredential {
                       aws_access_key_id :: Text
                     , aws_secret_access_key :: Text
                     }
                   deriving Generic
                   
instance Show AWSCredential where
  show _ = "AWSCredential"

instance FromJSON AWSCredential


data StorageConfig = StorageConfig {
                       storageAWS :: AWSCredential
                     , storagePath :: FilePath
                     , storageLocalPath :: FilePath
                     }
                   deriving (Generic,Show)

instance FromJSON StorageConfig
