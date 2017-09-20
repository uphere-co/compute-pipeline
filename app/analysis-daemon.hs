module Main where

import           Pipeline.App.AnalysisDaemon
import           Pipeline.Load
import           Pipeline.Type

main :: IO ()
main = do
  cfg <- (\ec -> case ec of {Left err -> error err;Right c -> return c;}) =<< loadConfigFile "config/config.json"    
  runDaemon (_corenlpstore cfg) (_errstore cfg) (_mgstore cfg) (_mgdotfigstore cfg) (_lexconfigpath cfg)
  -- mkBloombergMGFig
