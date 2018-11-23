module SO.Process
  ( mainProcess
  ) where

import           Control.Distributed.Process.Lifted ( Process )
import           Control.Monad.IO.Class ( liftIO )
------
import           CloudHaskell.Type        ( Pipeline )


mainProcess :: Pipeline ()
mainProcess = do
  liftIO $ putStrLn "mainProcess2"
