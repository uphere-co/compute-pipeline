module Main where

import Pipeline.App.CoreNLPRunner

main :: IO ()
main = runCoreNLPforNewsAPISource "bloomberg"
