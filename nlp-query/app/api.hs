module Main where

import           Pipeline.Operation.DB             (getConnection)
--
import           Query.App.API


main :: IO ()
main = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  getOneDayArticles conn
