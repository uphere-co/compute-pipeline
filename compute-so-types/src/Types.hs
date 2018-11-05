{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Types
  ( SOHandles(..)
  ) where

import Control.Concurrent.MVar ( MVar(..) )
import Control.DeepSeq         ( NFData )
import GHC.Generics            ( Generic )
import Network.Wai             ( Application )


-- | The set of functions that you want to expose from your shared object
data SOHandles = SOHandles
  { someApplication :: MVar Int -> Application
  } deriving (Generic, NFData)
