{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra

testM1 = [((10000,10000),1.5),((9999,9999),0.2)]

main :: IO ()
main = do
  print $ (mkSparse testM1)
  putStrLn "TF-IDF App"
