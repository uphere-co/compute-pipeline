{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage.Type where

newtype Path = LocalFile FilePath
          deriving (Show)


