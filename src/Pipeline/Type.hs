{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pipeline.Type where

import           Control.Lens
import           Options.Applicative
import           Data.Monoid                                ((<>))
import           Data.Text                        (Text)
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import           NewsAPI.DB
import qualified NewsAPI.DB.Article         as Ar
import           NewsAPI.Type                     (NewsAPIAnalysisDB(..))
--

type SentIdx = Int
type CharIdx = Int
type BeginEnd = (CharIdx,CharIdx)
type TagPos a = (CharIdx,CharIdx,a)
type SentItem = (SentIdx,BeginEnd,Text)

data ProgOption = ProgOption { dir :: FilePath
                             , entityFile :: FilePath
                             , dbname :: String
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strOption (long "entity" <> short 'e' <> help "Entity File")
                      <*> strOption (long "dbname" <> short 's' <> help "DB name")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "Named Entity Recognition")

data TaggedResult = TaggedResult { resultSUTime :: T.ListTimex
                                 , resultNER :: [(Int,Int,String)]
                                 , resultDoc :: D.Document
                                 }


data DoneAnalysis = DoneAnalysis
  { _done_corenlp :: Bool
  , _done_srl     :: Bool
  , _done_ner     :: Bool
  } deriving (Show)

makeLenses ''DoneAnalysis

mkNewsAPIAnalysisDB das article =
  NewsAPIAnalysisDB { analysis_sha256 = (Ar._sha256 article)
                    , analysis_source = (Ar._source article)
                    , analysis_corenlp = if das ^. done_corenlp then Just "y" else Nothing
                    , analysis_srl     = if das ^. done_srl then Just "y" else Nothing
                    , analysis_ner     = if das ^. done_ner then Just "y" else Nothing
                    , analysis_created = (Ar._created article)
                    }
