{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Type

main :: IO ()
main = do
  acfg <- O.execParser progOption
  cfg <- (\case {Left err -> error err;Right c -> return c;}) =<< loadConfigFile (acfg ^. configpath)
  conn <- getConnection (cfg ^. dbstring)
  runAnalysisAll cfg conn
