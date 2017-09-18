module Main where

import Pipeline.App.AnalysisDaemon

main :: IO ()
main = mkBloombergMGFig
  -- runDaemon
