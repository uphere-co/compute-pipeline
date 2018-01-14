{-# LANGUAGE DeriveGeneric #-}

module SemanticParserAPI.Compute.Type where

import           Data.Binary                    (Binary)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
--
import           SRL.Analyze.Type               (MGVertex,MGEdge,MeaningGraph)


data ComputeQuery = CQ_Text Text
                  deriving (Generic,Show)

instance Binary ComputeQuery


data ComputeResult = CR_TokenMeaningGraph [[(Int,Text)]] [MeaningGraph]
                   deriving (Generic,Show)

instance Binary ComputeResult
