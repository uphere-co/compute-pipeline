{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pipeline.Type where

import           Control.Lens
import           Options.Applicative
import           Data.Monoid                                 ((<>))
import           Data.Text                                   (Text)
import           Data.Time.Clock                             (NominalDiffTime,UTCTime)
--
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
--


data ProgOption = ProgOption { _configpath :: FilePath
                             } deriving Show

makeLenses ''ProgOption

data CoreNLPRunOption = CoreNLPRunOption
  { _configpath' :: FilePath
  , _btime       :: String
  , _etime       :: String
  } deriving Show

makeLenses ''CoreNLPRunOption

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "config" <> short 'c' <> help "Config JSON path")

cOptions :: Parser CoreNLPRunOption
cOptions = CoreNLPRunOption <$> strOption (long "config" <> short 'c' <> help "Config JSON path")
                            <*> strOption (long "btime" <> short 'b' <> help "Begin time")
                            <*> strOption (long "etime" <> short 'e' <> help "End time")

progOption :: ParserInfo ProgOption
progOption = info pOptions (fullDesc <> progDesc "NLP Pipeline")

corenlpRunOption :: ParserInfo CoreNLPRunOption
corenlpRunOption = info cOptions (fullDesc <> progDesc "CoreNLP Run")

data TaggedResult = TaggedResult { resultSUTime :: T.ListTimex
                                 , resultNER :: [(Int,Int,String)]
                                 , resultDoc :: D.Document
                                 }


data DoneAnalysis = DoneAnalysis
  { _done_corenlp :: Maybe Bool
  , _done_srl     :: Maybe Bool
  , _done_ner     :: Maybe Bool
  } deriving (Show)

makeLenses ''DoneAnalysis


nominalDay :: NominalDiffTime
nominalDay = 86400



data TimeConstraint = Between { _bTime :: UTCTime
                              , _eTime :: UTCTime
                              }
                    | After   { _bTime :: UTCTime
                              }
                    | Before  { _eTime :: UTCTime
                              }


type SourceTimeConstraint = (Maybe Text, Maybe TimeConstraint)

