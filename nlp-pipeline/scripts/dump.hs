{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe                              (catMaybes)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T.IO
import           Pipeline.Source.NewsAPI.Article

main :: IO ()
main = do
  articles <- getTimeTitleDescFromSrcWithHash "bloomberg"
  result' <- flip mapM (catMaybes articles) $ \(_,(hsh,ptime,title,desc)) -> do
    return desc
  let result = T.intercalate "\n" $ result'
  T.IO.writeFile "bloomberg.txt" result
