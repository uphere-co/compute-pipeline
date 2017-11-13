{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
--
import           NLP.Shared.Type                   (dbstring)
--
import           Pipeline.Load
import           Pipeline.Operation.DB             (getConnection)
--
import           Query.App.API
import           Query.Type


main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- loadConfigFile (acfg ^. configpath)
  conn <- getConnection (cfg ^. dbstring)
  let prt = acfg ^. port
  run conn cfg prt
