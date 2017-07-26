{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.NewsAPI.Article where

import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Char8      as B   
import qualified Data.ByteString.Lazy.Char8 as L8  
import           Data.Foldable                     (toList)
import           Data.List                         (sort)
import           Data.Text                         (Text)
import qualified Data.Text                  as T   
import qualified Database.PostgreSQL.Simple as PGS 
import           System.Directory                  (doesFileExist)
import           System.Directory.Tree             (dirTree,readDirectoryWith)
--
import           NewsAPI.DB
import qualified NewsAPI.DB.Article         as A
import           NewsAPI.Type

getTimeTitleDescFromSrc :: String -> IO [Maybe (Text, Text, Text)]
getTimeTitleDescFromSrc src = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  articles <- getArticleBySource src conn
  result <- flip mapM articles $ \x -> do
    let hsh = T.unpack $ A._content_hash x
        fileprefix = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/" ++ src ++ "/"
        filepath = fileprefix ++ hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        getTimeTitleDescFromByteString bstr
      False -> print hsh >> error "error"
  PGS.close conn
  return result

getTimeTitleDescFromByteString :: Monad m => B.ByteString -> m (Maybe (Text, Text, Text))
getTimeTitleDescFromByteString bstr = do
  let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
  case esrc of
    Left  _   -> return Nothing
    Right src -> return ((,,) <$> _publishedAt src <*> _title src <*> _description src)

getDescription :: FilePath -> IO Text
getDescription f = do
    bstr <- B.readFile f
    let ea = eitherDecodeStrict bstr :: Either String SourceArticles
    case ea of
      Left  _ -> return ""
      Right a -> return (maybe "" id (_description a))

getFileList :: FilePath -> IO ([FilePath])
getFileList fp = do
  list' <- readDirectoryWith return fp
  let filelist = sort . toList $ dirTree list'
  return filelist
