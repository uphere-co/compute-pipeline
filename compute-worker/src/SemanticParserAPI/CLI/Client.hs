{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.CLI.Client where

import           Control.Distributed.Process.Lifted  (Process,SendPort)
import           Control.Monad                       (join,when)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Loops                 (whileJust_)
import           Control.Monad.Trans.Class           (lift)
import           Data.List                           (intercalate)
import qualified Data.Text                     as T
import           System.Console.Haskeline            (runInputT,getInputLine,defaultSettings)
import           System.Console.Haskeline.MonadException (MonadException(controlIO),RunIO(..))
--
import           CloudHaskell.Util                   (LogProcess,queryProcess)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..))

instance MonadException Process where
  controlIO f = join . liftIO $ f (RunIO return)


consoleClient :: SendPort (ComputeQuery, SendPort ComputeResult) -> LogProcess ()
consoleClient sc = do
  runInputT defaultSettings $
    whileJust_ (getInputLine "% ") $ \input' ->
      when (not (null input')) $ do
        let w:ws = words input'
        case w of
          ":v" -> let input = T.pack (intercalate " " ws)
                  in lift $ queryProcess sc (CQ_Sentence input) (liftIO . print)
          ":r" -> lift $ queryProcess sc (CQ_Reuters 100) (liftIO . print)
          _ -> return ()



