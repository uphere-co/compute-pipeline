{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Pipeline.Application.Run
-- import Pipeline.Application.RunPropBank
import Pipeline.Application.RunBunch

main :: IO ()
main = runWikiEL

-- runWikiEL -- print =<< getPBFull "Trump canceled Paris."
