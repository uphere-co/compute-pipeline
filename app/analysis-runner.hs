module Main where

import System.Environment           (getArgs)
--
import Pipeline.App.AnalysisRunner

main :: IO ()
main = do
  runAnalysisAll
