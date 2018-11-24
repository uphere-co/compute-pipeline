{-# LANGUAGE OverloadedStrings #-}
module SO.Handle
  ( hsNewSOHandle
  ) where

import           Control.Concurrent.STM ( newTVarIO )
import           Foreign                ( StablePtr, newStablePtr )
import           Worker.Type            ( SOHandle(..) )
------
import           SO.Handler.Web         ( webApp )
import           SO.Handler.Worker      ( workerMain )


foreign export ccall "hs_soHandle"
  hsNewSOHandle :: IO (StablePtr SOHandle)

hsNewSOHandle :: IO (StablePtr SOHandle)
hsNewSOHandle = do
  ref_count <- newTVarIO (0 :: Int)
  newStablePtr SOHandle
               { soApplication = webApp ref_count
               , soProcess = workerMain ref_count
               }
