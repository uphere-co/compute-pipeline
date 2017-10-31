{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens               ((^.))
import qualified Options.Applicative as O
--
import           Pipeline.App.CoreNLPDaemon (runDaemon)
import           Pipeline.Load              (loadConfigFile)
import           Pipeline.Type              (configpath,progOption)

main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- (\case {Left err -> error err;Right c -> return c;}) =<< loadConfigFile (acfg ^. configpath)
  runDaemon cfg
