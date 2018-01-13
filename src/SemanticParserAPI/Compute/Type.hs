{-# LANGUAGE DeriveGeneric #-}

module SemanticParserAPI.Compute.Type where

import           Data.Binary
import           Data.Text    (Text)
import           GHC.Generics

data ComputeQuery = CQ_Text Text
                  deriving (Generic,Show)

instance Binary ComputeQuery


data ComputeResult = CR_Text Text
                   deriving (Generic,Show)

instance Binary ComputeResult

