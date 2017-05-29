{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv           as Csv
import           Data.List                 (sortOn)
import qualified Data.Text.IO       as TIO
import           Data.Text                 (Text)
import qualified Data.Text          as T
import           System.Directory          (listDirectory)
import           System.FilePath           (takeBaseName)

main :: IO ()
main = do
  filelist <- listDirectory "contents"
  result <- flip mapM filelist $ \f -> do
    txt <- TIO.readFile ("contents/" ++ f)
    return (takeBaseName f,txt)
  BL.writeFile "result.csv" $ Csv.encode (sortOn (\(x,y) -> x) result)
