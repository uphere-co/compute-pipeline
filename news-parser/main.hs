{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable         (toList)
import           Data.List             (sort)
import           System.Directory.Tree (dirTree, readDirectoryWith)

main :: IO ()
main = do
  filelist' <- readDirectoryWith return "/data/modori/NYT.dump"
  let filelist = sort . toList $ dirTree filelist'
  print filelist
