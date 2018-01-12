{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import SemanticParserAPI.Compute (computeMain)

main :: IO ()
main = computeMain

