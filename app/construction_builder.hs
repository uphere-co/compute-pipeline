{-# LANGUAGE OverloadedStrings #-}

module Main where

--
import Pipeline.Application.Construction

main :: IO ()
main = runConstructionWithText "Floyd Mayweather, Conor McGregor trade insults as press tour begins."
  
