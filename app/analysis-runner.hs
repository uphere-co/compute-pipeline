module Main where

import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Type

main :: IO ()
main = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  cfg <- (\ec -> case ec of {Left err -> error err;Right c -> return c;}) =<< loadConfigFile "config/config.json"
  runAnalysisAll conn (_corenlpstore cfg) (_mgstore cfg) (_mgdotfigstore cfg) (_arbstore cfg) (_lexconfigpath cfg)
