{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Storage.Operation where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.UUID.V4               (nextRandom)
--
import Storage.Config             (StorageConfig)

register :: StorageConfig -> ExceptT String IO ()
register cfg = do
  liftIO $ putStrLn "register"
  liftIO $ do i <- nextRandom
              print i
  liftIO $ print cfg

install :: StorageConfig -> ExceptT String IO ()
install cfg = do
  liftIO $ putStrLn "install"
  liftIO $ print cfg

