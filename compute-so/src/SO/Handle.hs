{-# LANGUAGE OverloadedStrings #-}
module SO.Handle
  ( hsNewSOHandle
  ) where

import Control.Concurrent ( newMVar )
import Foreign            ( StablePtr, newStablePtr )
import Worker.Type        ( SOHandle(..) )
------
import SO.Handler.Web     ( webApp )
import SO.Handler.Worker  ( workerMain )


foreign export ccall "hs_soHandle"
  hsNewSOHandle :: IO (StablePtr SOHandle)

hsNewSOHandle :: IO (StablePtr SOHandle)
hsNewSOHandle = do
  countRef <- newMVar (0 :: Int)
  newStablePtr SOHandle
               { soApplication = webApp countRef
               , soProcess = workerMain
               }
