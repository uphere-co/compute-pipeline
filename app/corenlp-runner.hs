module Main where

import System.Environment           (getArgs)
--
import Pipeline.App.CoreNLPRunner

main :: IO ()
main = do
  [src] <- getArgs
  runCoreNLPforNewsAPISource src
