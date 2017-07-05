{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Pipeline.Application.Run
-- import Pipeline.Application.RunPropBank
import Pipeline.Application.RunBunch
import Pipeline.Application.Tokenizer

main :: IO ()
main = runTokenizer

-- runWikiEL -- print =<< getPBFull "Trump canceled Paris."
