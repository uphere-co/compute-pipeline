{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
--
import           NLP.Shared.Type                   (dbstring)
--
import           Pipeline.Load
import           Pipeline.Operation.DB             (getConnection)
import           Pipeline.Type
--
import           Query.App.API

main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- (\case {Left err -> error err;Right c -> return c;}) =<< loadConfigFile (acfg ^. configpath)
  conn <- getConnection (cfg ^. dbstring)
  run conn cfg
