{-# LANGUAGE DeriveGeneric #-}

module SemanticParserAPI.Compute.Type where

import           Control.DeepSeq                (NFData)
import           Data.Aeson
import           Data.Binary                    (Binary)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
--
import           SRL.Analyze.Type               (MGVertex,MGEdge,MeaningGraph)


data ComputeQuery = CQ_Text Text
                  deriving (Generic,Show)

instance Binary ComputeQuery
instance ToJSON ComputeQuery
instance FromJSON ComputeQuery
instance NFData ComputeQuery

data ComputeResult = CR_TokenMeaningGraph [[(Int,Text)]] [MeaningGraph]
                   deriving (Generic,Show)

instance Binary ComputeResult
instance ToJSON ComputeResult
instance FromJSON ComputeResult
instance NFData ComputeResult
