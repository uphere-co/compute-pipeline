{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.CLI.Client where

import           Control.Distributed.Process.Lifted  (Process,SendPort)
import           Control.Monad                       (join)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Loops                 (whileJust_)
import           Control.Monad.Trans.Class           (lift)
import qualified Data.Text                     as T
import           System.Console.Haskeline            (runInputT,getInputLine,defaultSettings)
import           System.Console.Haskeline.MonadException (MonadException(controlIO),RunIO(..))
--
import           CloudHaskell.Util                   (LogProcess,tellLog,queryProcess)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..))

instance MonadException Process where
  controlIO f = join . liftIO $ f (RunIO return)


consoleClient :: SendPort (ComputeQuery, SendPort ComputeResult) -> LogProcess ()
consoleClient sc = do
  runInputT defaultSettings $
    whileJust_ (getInputLine "% ") $ \input' -> do
      -- liftIO $ print input'
      lift $ queryProcess sc (CQ_Text (T.pack input')) (liftIO . print)




