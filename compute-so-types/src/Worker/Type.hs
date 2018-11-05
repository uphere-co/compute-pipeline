{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Worker.Type
  ( SOHandle(..)
  ) where

import Control.DeepSeq         ( NFData )
import GHC.Generics            ( Generic )
import Network.Wai             ( Application )


-- | The set of functions that you want to expose from your shared object
data SOHandle = SOHandle
                { soApplication :: Application
                }
              deriving (Generic, NFData)
