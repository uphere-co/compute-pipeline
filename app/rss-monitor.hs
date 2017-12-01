module Main where

import           Control.Lens             ((^.))
import qualified Options.Applicative as O
--
import           Pipeline.App.RSSMonitor
import           Pipeline.Load
import           Pipeline.Type

main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- loadConfigFile (acfg ^. configpath)
  runWithCoreNLP cfg
