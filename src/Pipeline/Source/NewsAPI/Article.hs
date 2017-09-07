{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Pipeline.Source.NewsAPI.Article where

import           Control.Lens
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B   
import qualified Data.ByteString.Lazy.Char8 as L8  
import           Data.Foldable                     (toList)
import           Data.List                         (sort)
import           Data.Text                         (Text)
import qualified Data.Text                  as T   
import           Data.Time.Clock                   (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS 
import           System.Directory                  (doesFileExist)
import           System.Directory.Tree             (dirTree,readDirectoryWith)
import           System.FilePath                   ((</>))
--
import           NewsAPI.DB
import qualified NewsAPI.DB.Article         as Ar
import           NewsAPI.Type
--
import           Pipeline.Operation.DB


type NewsAPIArticleContent = (Text, Text, Text, Text)

getHashByTime :: UTCTime -> IO [(Text,Text)]
getHashByTime time = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  articles <- getArticleByTime time conn
  return (map (\x -> (Ar._source x, T.pack $ L8.unpack $ L8.fromStrict $ B16.encode $ Ar._sha256 x)) articles)

getTimeTitleDescFromSrcWithHash :: String -> IO [Maybe (Ar.ArticleH,NewsAPIArticleContent)]
getTimeTitleDescFromSrcWithHash src = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  articles <- getArticleBySource src conn
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._sha256 x
        fileprefix = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/" ++ src ++ "/"
        filepath = fileprefix ++ hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        content <- getTimeTitleDescFromByteStringWithHash bstr hsh
        return ((,) <$> Just x <*> content)
      False -> print hsh >> error "error"
  PGS.close conn
  return result

getTimeTitleDescFromByteStringWithHash :: Monad m => B.ByteString -> String -> m (Maybe NewsAPIArticleContent)
getTimeTitleDescFromByteStringWithHash bstr str = do
  let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
  case esrc of
    Left  _   -> return Nothing
    Right src -> return ((,,,) <$> Just (T.pack str) <*> _publishedAt src <*> _title src <*> _description src)

getDescription :: FilePath -> IO Text
getDescription f = do
    bstr <- B.readFile f
    let ea = eitherDecodeStrict bstr :: Either String SourceArticles
    case ea of
      Left  _ -> return ""
      Right a -> return (maybe "" id (_description a))


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
