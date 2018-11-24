module SO.Handler.Process
  ( mainProcess
  ) where

import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.STM   ( TVar, readTVarIO )
import           Control.Monad            ( forever )
import           Control.Monad.IO.Class   ( liftIO )
------
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )


mainProcess :: TVar Int -> Pipeline ()
mainProcess ref_count = do
  tellLog "mainProcess2"
  forever $ do
    liftIO $ threadDelay 3000000
    c <- liftIO $ readTVarIO ref_count
    tellLog $ "c = " ++ show c
