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
import qualified DB.Schema.NewsAPI.Analysis            as NewsAPI
import qualified DB.Schema.NewsAPI.Article             as NewsAPI
import qualified DB.Schema.NewsAPI.ArticleError        as NewsAPI
import qualified DB.Schema.RSS.Analysis                as RSS
import qualified DB.Schema.RSS.Article                 as RSS
import qualified DB.Schema.RSS.ErrorArticle            as RSS
import           DB.Type
-- import           NewsAPI.Type                                (NewsAPIArticleErrorDB(..),NewsAPIAnalysisDB(..))
import           NLP.Shared.Type
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

mkNewsAPIAnalysis :: DoneAnalysis -> NewsAPI.Article -> NewsAPI.Analysis
mkNewsAPIAnalysis das article = undefined
{-  NewsAPI.Analysis { analysis_hash = Ar._hash article
                    , analysis_source = Ar._source article
                    , analysis_corenlp = das ^. done_corenlp
                    , analysis_srl     = das ^. done_srl
                    , analysis_ner     = das ^. done_ner
                    , analysis_created = Ar._created article
                    }
-}

mkNewsAPIArticleError :: NewsAPI.Article -> NewsAPI.ArticleError
mkNewsAPIArticleError article = undefined
{-  NewsAPIArticleErrorDB { article_error_hash = Ar._hash article
                        , article_error_source = Ar._source article
                        , article_error_created = Ar._created article
                        }
-}

mkRSSAnalysis :: DoneAnalysis -> RSS.RSSArticle -> RSS.RSSAnalysis
mkRSSAnalysis das article = undefined
{-  RSSAnalysisDB { _rss_analysis_hash    = RAr._hash article
                , _rss_analysis_source  = RAr._source article
                , _rss_analysis_corenlp = das ^. done_corenlp
                , _rss_analysis_srl     = das ^. done_srl
                , _rss_analysis_ner     = das ^. done_ner
                , _rss_analysis_created = RAr._created article
                }
-}

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

{-
  { _source :: Maybe Text
  , _bTime  :: Maybe UTCTime
  , _eTime  :: Maybe UTCTime
  } deriving (Show)
-}
