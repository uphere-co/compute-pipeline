{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
--
import           Pipeline.App.AnalysisDaemon
import           Pipeline.Load
import           Pipeline.Type

main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- (\case {Left err -> error err;Right c -> return c;}) =<< loadConfigFile (acfg ^. configpath) -- "config/config.json"
  runDaemon cfg
  -- mkBloombergMGFig cfg
