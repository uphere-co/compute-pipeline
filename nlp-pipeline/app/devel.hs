{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Pipeline.Application.Run
-- import Pipeline.Application.RunPropBank

main :: IO ()
main = runWikiEL -- print =<< getPBFull "Trump canceled Paris."
