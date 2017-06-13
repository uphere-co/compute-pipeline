{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                    (foldl')
import qualified Data.Map              as M
-- import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Numeric.LinearAlgebra

testM1 = [((10000,10000),1.5),((9999,9999),0.2)]

main :: IO ()
main = do
  txt <- TIO.readFile "test.txt"
  print txt
  let result = foldl' (\acc x -> M.insertWith' (+) x 1 acc) M.empty (T.words txt)
  -- print $ (mkSparse testM1)
  print result
  putStrLn "TF-IDF App"
