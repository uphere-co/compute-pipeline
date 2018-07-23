{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module SemanticParserAPI.Compute.Type where

import           Control.DeepSeq                (NFData)
import           Data.Aeson                     (FromJSON,ToJSON)
import           Data.Binary                    (Binary)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)
--
import           SRL.Analyze.Type               (MeaningGraph,ConsoleOutput)

data NetworkConfig = NetworkConfig {
                       hostg :: Text
                     , hostl :: Text
                     , port :: Int
                     }
                   deriving (Generic,Show,FromJSON,ToJSON)

data CellConfig = CellConfig {
                    cellName :: Text
                  , cellAddress :: NetworkConfig
                  }
                deriving (Generic,Show,FromJSON,ToJSON)

data ComputeConfig = ComputeConfig {
                       computeServer :: NetworkConfig
                     , computeWeb :: NetworkConfig
                     , computeCells :: [CellConfig]
                     , computeBypassNER :: Bool
                     , computeBypassTEXTNER :: Bool
                     }
                   deriving (Generic,Show,FromJSON,ToJSON)

data ComputeQuery = CQ_Sentence Text
                  | CQ_Reuters Int
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data ResultSentence = ResultSentence { _sentence_query :: Text
                                     , _sentence_token :: [[(Int,Text)]]
                                     , _sentence_meaning_graph :: [MeaningGraph]
                                     , _sentence_output :: ConsoleOutput
                                     }
                    deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data ResultReuters = ResultReuters { _reuters_query :: Int
                                   , _reuters_mgs :: [MeaningGraph]
                                   }
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data ComputeResult = CR_Sentence ResultSentence
                   | CR_Reuters  ResultReuters
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data StatusQuery = SQ
                  deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)

data StatusResult = SR
                   deriving (Generic,Show,Binary,ToJSON,FromJSON,NFData)
