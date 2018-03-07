{-# LANGUAGE DeriveGeneric #-}

module SemanticParserAPI.Compute.Type where

import           Control.DeepSeq                (NFData)
import           Data.Aeson
import           Data.Binary                    (Binary)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
--
import           SRL.Analyze.Type               (MeaningGraph)


data ComputeQuery = CQ_Sentence Text
                  | CQ_Reuters Int
                  deriving (Generic,Show)

instance Binary   ComputeQuery
instance ToJSON   ComputeQuery
instance FromJSON ComputeQuery
instance NFData   ComputeQuery

data ResultSentence = ResultSentence { _sentence_query :: Text
                                     , _sentence_token :: [[(Int,Text)]]
                                     , _sentence_meaning_graph :: [MeaningGraph]
                                     }
                    deriving (Generic,Show)

instance Binary   ResultSentence
instance ToJSON   ResultSentence
instance FromJSON ResultSentence
instance NFData   ResultSentence

data ResultReuters = ResultReuters { _reuters_query :: Int
                                   , _reuters_mgs :: [MeaningGraph]
                                   }
                   deriving (Generic,Show)

instance Binary   ResultReuters
instance ToJSON   ResultReuters
instance FromJSON ResultReuters
instance NFData   ResultReuters


data ComputeResult = CR_Sentence ResultSentence
                   | CR_Reuters  ResultReuters
                   deriving (Generic,Show)

instance Binary   ComputeResult
instance ToJSON   ComputeResult
instance FromJSON ComputeResult
instance NFData   ComputeResult
