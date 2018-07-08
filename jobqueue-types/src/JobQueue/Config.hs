{-# LANGAUGE DeriveGeneric #-}
module JobQueue.Config where

import GHC.Generics (Generic)

data ClientConfiguration = ClientConfiguration { 
  computerName :: String, 
  haveMathematica :: Bool,
  havePBS :: Bool, 
  canMonteCarlo :: Bool, 
  datasetDir :: String
} deriving (Show,Eq,Ord,Generic)

instance FromJSON ClientConfiguration
instance ToJSON ClientConfiguration


 



