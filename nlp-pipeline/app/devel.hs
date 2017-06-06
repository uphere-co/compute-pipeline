{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Pipeline.Application.RunPropBank

main :: IO ()
main = print =<< getPBFull "Trump canceled Paris."
