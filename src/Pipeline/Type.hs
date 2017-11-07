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
import qualified DB.Schema.NewsAPI.Article             as Ar
import qualified DB.Schema.RSS.Article                 as RAr
import           DB.Type
import           NewsAPI.Type                                (NewsAPIArticleErrorDB(..),NewsAPIAnalysisDB(..))
import           NLP.Shared.Type
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
  NewsAPIAnalysisDB { analysis_hash = Ar._hash article
                    , analysis_source = Ar._source article
                    , analysis_corenlp = das ^. done_corenlp
                    , analysis_srl     = das ^. done_srl
                    , analysis_ner     = das ^. done_ner
                    , analysis_created = Ar._created article
                    }

mkNewsAPIArticleErrorDB :: Ar.ArticleP a ByteString Text UTCTime -> NewsAPIArticleErrorDB
mkNewsAPIArticleErrorDB article =
  NewsAPIArticleErrorDB { article_error_hash = Ar._hash article
                        , article_error_source = Ar._source article
                        , article_error_created = Ar._created article
                        }

mkRSSAnalysisDBInfo das article =
  RSSAnalysisDB { _rss_analysis_hash    = RAr._hash article
                , _rss_analysis_source  = RAr._source article
                , _rss_analysis_corenlp = das ^. done_corenlp
                , _rss_analysis_srl     = das ^. done_srl
                , _rss_analysis_ner     = das ^. done_ner
                , _rss_analysis_created = RAr._created article
                }

nominalDay :: NominalDiffTime
nominalDay = 86400

data SourceConstraint = SourceConstraint
  { _source :: Maybe Text
  , _bTime  :: Maybe UTCTime
  , _eTime  :: Maybe UTCTime
  } deriving (Show)

tempPC = PathConfig
  { _corenlpstore  = "/home/modori/data/production/corenlp"
  , _mgstore       = "/home/modori/data/production/mgs"
  , _mgdotfigstore = "/home/modori/data/production/mgdotgraphs"
  , _lexconfigpath = "/home/modori/repo/src/lexicon-builder/config_global.json"
  , _arbstore      = "/home/modori/data/production/arbs"
  , _errstore      = "/home/modori/data/production/errs"
  , _dbstring      = "dbname=mydb2 host=localhost port=65432 user=modori"
  , _newsapistore  = "/data/groups/uphere/repo/fetchfin/newsapi/Articles"
  , _nytstore      = "/data/groups/uphere/news-archive/fetchfin/nyt/NYTArticles"
  , _rssstore      = "/home/modori/temp/RSS"
  }
