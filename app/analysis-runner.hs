module Main where

import System.Environment           (getArgs)
--
import Pipeline.App.AnalysisRunner
import Pipeline.Operation.DB

main :: IO ()
main = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  runAnalysisAll conn
