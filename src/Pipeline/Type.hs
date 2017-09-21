{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pipeline.Type where

import           Control.Lens
import           Options.Applicative
import           Data.Aeson
import           Data.Aeson.Types                            (typeMismatch)
import           Data.ByteString.Char8                       (ByteString)
import           Data.Monoid                                 ((<>))
import           Data.Text                                   (Text)
import           Data.Time.Clock                             (NominalDiffTime,UTCTime)
import           GHC.Generics
--
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified NewsAPI.DB.Article                    as Ar
import           NewsAPI.Type                                (NewsAPIArticleErrorDB(..),NewsAPIAnalysisDB(..))
--


data ProgOption = ProgOption { _configpath :: FilePath
                             } deriving Show

makeLenses ''ProgOption

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "config" <> short 'c' <> help "Config JSON path")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "NLP Pipeline")

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

mkNewsAPIAnalysisDB :: DoneAnalysis -> Ar.ArticleP a ByteString Text UTCTime -> NewsAPIAnalysisDB
mkNewsAPIAnalysisDB das article =
  NewsAPIAnalysisDB { analysis_sha256 = Ar._sha256 article
                    , analysis_source = Ar._source article
                    , analysis_corenlp = das ^. done_corenlp
                    , analysis_srl     = das ^. done_srl
                    , analysis_ner     = das ^. done_ner
                    , analysis_created = Ar._created article
                    }

mkNewsAPIArticleErrorDB :: Ar.ArticleP a ByteString Text UTCTime -> NewsAPIArticleErrorDB
mkNewsAPIArticleErrorDB article =
  NewsAPIArticleErrorDB { article_error_hash = Ar._sha256 article
                        , article_error_source = Ar._source article
                        , article_error_created = Ar._created article
                        }


nominalDay :: NominalDiffTime
nominalDay = 86400

data PathConfig = PathConfig
  { _corenlpstore  :: FilePath
  , _mgstore       :: FilePath
  , _mgdotfigstore :: FilePath
  , _lexconfigpath :: FilePath
  , _arbstore      :: FilePath
  , _errstore      :: FilePath
  } deriving (Show, Generic)

makeLenses ''PathConfig

instance ToJSON PathConfig

instance FromJSON PathConfig where
  parseJSON (Object o) =
    PathConfig <$> o .: "CoreNLPStore"
               <*> o.: "MeaningGraphStore"
               <*> o.: "MGDotFigStore"
               <*> o.: "LexDataConfigPath"
               <*> o.: "ARBStore"
               <*> o.: "ErrorArticleStore"
  parseJSON invalid = typeMismatch "PathConfig" invalid
