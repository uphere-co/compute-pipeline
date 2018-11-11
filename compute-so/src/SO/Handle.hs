{-# LANGUAGE OverloadedStrings #-}
module SO.Handle
  ( hsNewSOHandle
  ) where

import Control.Concurrent ( newMVar )
import Foreign            ( StablePtr(..), newStablePtr )
import Worker.Type        ( SOHandle(..) )
------
import SO.MyCode ( myApp, workerMain )


foreign export ccall "hs_soHandle"
  hsNewSOHandle :: IO (StablePtr SOHandle)

hsNewSOHandle :: IO (StablePtr SOHandle)
hsNewSOHandle = do
  countRef <- newMVar (0 :: Int)
  newStablePtr SOHandle
               { soApplication = myApp countRef
               , soProcess = workerMain
               }
