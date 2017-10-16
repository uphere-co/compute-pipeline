{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.NewsAPI.Article where

import           Control.Lens                      ((^.))
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B   
import qualified Data.ByteString.Lazy.Char8 as L8  
import           Data.Text                         (Text)
import qualified Data.Text                  as T   
import           Data.Time.Clock                   (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS 
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import           NewsAPI.DB
import qualified DB.Schema.NewsAPI.Article         as Ar
import           NewsAPI.Type
import           NLP.Shared.Type                   (PathConfig,dbstring,newsapistore)
--
import           Pipeline.Operation.DB
import           Pipeline.Type


type NewsAPIArticleContent = (Text, Text, Text, Text)

getHashByTime :: PathConfig -> UTCTime -> IO [(Text,Text)]
getHashByTime cfg time = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- getArticleByTime time conn
  PGS.close conn
  return (map (\x -> (Ar._source x, T.pack $ L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x)) articles)

getTimeTitleDescFromSrcWithHash :: PathConfig -> String -> IO [Maybe (Ar.ArticleH,NewsAPIArticleContent)]
getTimeTitleDescFromSrcWithHash cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- getArticleBySource src conn
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x
        fileprefix = (cfg ^. newsapistore) </> src
        filepath = fileprefix </> hsh
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

getTitle :: FilePath -> IO Text
getTitle fp = do
  bstr <- B.readFile fp
  let ea = eitherDecodeStrict bstr :: Either String SourceArticles
  case ea of
    Left  _ -> return ""
    Right a -> return (maybe "" id (_title a))
