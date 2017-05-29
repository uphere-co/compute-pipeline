{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Lens       hiding (children,element)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                 (sortOn)
import qualified Data.List.Split    as DLS
import           Data.Maybe                (mapMaybe)
import qualified Data.Text.IO       as TIO
import           Data.Text                 (Text)
import qualified Data.Text          as T
import           Data.Text.Lazy     as TL
import           System.Directory          (doesFileExist, listDirectory)
import           System.Environment
import           System.FilePath           (takeBaseName)
import           System.IO
--
import qualified Data.Csv           as Csv


main :: IO ()
main = do
  filelist <- listDirectory "contents"
  result <- flip mapM filelist $ \f -> do
    txt <- TIO.readFile ("contents/" ++ f)
    return (takeBaseName f,txt)
  BL.writeFile "result.csv" $ Csv.encode (sortOn (\(x,y) -> x) result)
