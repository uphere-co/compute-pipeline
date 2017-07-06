{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment              (getArgs)
--
import Pipeline.Application.Run
-- import Pipeline.Application.RunPropBank
import Pipeline.Application.RunBunch
import Pipeline.Application.Tokenizer



main :: IO ()
main = do
  (n :: Int) <- (read . (!! 0)) <$> getArgs
  runTokenizer n

-- runWikiEL -- print =<< getPBFull "Trump canceled Paris."
