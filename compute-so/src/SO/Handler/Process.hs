module SO.Handler.Process
  ( mainProcess
  ) where

import           Control.Monad.IO.Class ( liftIO )
------
import           CloudHaskell.Type        ( Pipeline )


mainProcess :: Pipeline ()
mainProcess = do
  liftIO $ putStrLn "mainProcess2"
