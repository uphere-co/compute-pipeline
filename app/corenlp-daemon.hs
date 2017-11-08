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
  cfg <- loadConfigFile (acfg ^. configpath)
  runDaemon cfg
