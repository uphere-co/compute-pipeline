{-# LANGUAGE OverloadedStrings #-}
module SO.Handles
  ( hsNewSOHandle
  ) where

import Foreign   ( StablePtr(..), newStablePtr )
import Types     ( SOHandles(..) )
import SO.MyCode ( myApp )


foreign export ccall "hs_soHandles"
  hsNewSOHandle :: IO (StablePtr SOHandles)


hsNewSOHandle :: IO (StablePtr SOHandles)
hsNewSOHandle = newStablePtr SOHandles
  { someApplication = myApp
  }
