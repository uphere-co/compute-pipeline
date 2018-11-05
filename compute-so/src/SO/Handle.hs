{-# LANGUAGE OverloadedStrings #-}
module SO.Handle
  ( hsNewSOHandle
  ) where

import Foreign     ( StablePtr(..), newStablePtr )
import Worker.Type ( SOHandle(..) )
------
import SO.MyCode ( myApp )


foreign export ccall "hs_soHandles"
  hsNewSOHandle :: IO (StablePtr SOHandle)


hsNewSOHandle :: IO (StablePtr SOHandle)
hsNewSOHandle =
  newStablePtr SOHandle
               { soApplication = myApp
               }
