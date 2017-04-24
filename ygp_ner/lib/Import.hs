{-# LANGUAGE OverloadedStrings #-}

module Import where

import           Data.Text      (Text)

data Dictionary = Dictionary {
  word :: Text
  } deriving (Show)
                              
