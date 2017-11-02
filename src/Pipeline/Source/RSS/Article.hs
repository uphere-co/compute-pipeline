{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.RSS.Article where

import           Control.Lens                      ((^.))
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B   
import qualified Data.ByteString.Lazy.Char8 as L8  
import           Data.Text                         (Text)
import qualified Data.Text                  as T   
import           Data.Time.Clock                   (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS 
import           Opaleye                           (runQuery)
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import           DB.Operation
import qualified DB.Schema.RSS.Article      as Ar
import           NLP.Shared.Type
import           RSS.Type                          (itempath)
--
import           Pipeline.Operation.DB
import           Pipeline.Type


getHashByTime :: PathConfig -> UTCTime -> IO [(Text,Text)]
getHashByTime cfg time = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- getRSSArticleByTime conn time
  PGS.close conn
  return (map (\x -> (Ar._source x, T.pack $ L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x)) articles)

getRSSArticleBySrc :: PathConfig -> String -> IO [Maybe (Ar.RSSArticleH,ItemRSS)]
getRSSArticleBySrc cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  articles' <- getRSSArticleBySource conn src
  let articles = articles' -- filter (\x -> ) articles'
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x
        fileprefix = (cfg ^. rssstore) </> src
        filepath = fileprefix </> itempath </> hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        content <- loadItemRSS bstr
        return ((,) <$> Just x <*> content)
      False -> putStrLn ("Following article exists in DB, but does not exist on disk : " ++ hsh) >> return Nothing -- error "error"
  PGS.close conn
  return result

getRSSArticleBtwnTime :: PathConfig -> UTCTime -> UTCTime -> IO [Maybe (Ar.RSSArticleH,ItemRSS)]
getRSSArticleBtwnTime cfg time1 time2 = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- runQuery conn (queryRSSArticleBetweenTime time1 time2)
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x
        src = T.unpack $ Ar._source x
        fileprefix = (cfg ^. rssstore) </> src
        filepath = fileprefix </> itempath </> hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        content <- loadItemRSS bstr
        return ((,) <$> Just x <*> content)
      False -> putStrLn ("Following article exists in DB, but does not exist on disk : " ++ hsh) >> return Nothing -- error "error"
  PGS.close conn
  return result

loadItemRSS :: B.ByteString -> IO (Maybe ItemRSS)
loadItemRSS bstr = do
  let esrc = eitherDecodeStrict bstr :: Either String ItemRSS
  case esrc of
    Left  _   -> return Nothing
    Right src -> return (Just src)
