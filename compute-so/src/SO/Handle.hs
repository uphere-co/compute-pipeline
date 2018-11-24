{-# LANGUAGE OverloadedStrings #-}
module SO.Handle
  ( hsNewSOHandle
  ) where

import           Control.Concurrent.STM  ( newTVarIO )
import           Foreign                 ( StablePtr, newStablePtr )
import           Worker.Type             ( SOHandle(..) )
------
import           CloudHaskell.QueryQueue ( emptyQQ )
------
import           SO.Handler.Web          ( webApp )
import           SO.Handler.Worker       ( workerMain )


foreign export ccall "hs_soHandle"
  hsNewSOHandle :: IO (StablePtr SOHandle)

hsNewSOHandle :: IO (StablePtr SOHandle)
hsNewSOHandle = do
  qqvar <- newTVarIO emptyQQ
  newStablePtr SOHandle
               { soApplication = webApp qqvar
               , soProcess     = workerMain qqvar
               }
