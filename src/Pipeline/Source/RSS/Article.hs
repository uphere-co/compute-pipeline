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
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import           NLP.Shared.Type
import           DB.Operation
import qualified DB.Schema.RSS.Article      as Ar
import           RSS.Type                          (ItemRSS(..))
--
import           Pipeline.Operation.DB
import           Pipeline.Type


type RSSArticleContent = (Text, UTCTime, Text, Text)

getHashByTime :: PathConfig -> UTCTime -> IO [(Text,Text)]
getHashByTime cfg time = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- getRSSArticleByTime conn time
  PGS.close conn
  return (map (\x -> (Ar._source x, T.pack $ L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x)) articles)

getTimeTitleDescFromSrcWithHash :: PathConfig -> String -> IO [Maybe (Ar.RSSArticleH,RSSArticleContent)]
getTimeTitleDescFromSrcWithHash cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- getRSSArticleBySource conn src
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x
        fileprefix = (cfg ^. rssstore) </> src
        filepath = fileprefix </> "articles" </> hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        content <- getTimeTitleDescFromByteStringWithHash bstr hsh
        return ((,) <$> Just x <*> content)
      False -> print hsh >> error "error"
  PGS.close conn
  return result

getTimeTitleDescFromByteStringWithHash :: Monad m => B.ByteString -> String -> m (Maybe RSSArticleContent)
getTimeTitleDescFromByteStringWithHash bstr str = do
  let esrc = eitherDecodeStrict bstr :: Either String ItemRSS
  case esrc of
    Left  _   -> return Nothing
    Right src -> return ((,,,) <$> Just (T.pack str) <*> Just (_pubDate src) <*> Just (_title src) <*> Just (_description src))
