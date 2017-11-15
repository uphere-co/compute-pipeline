{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.RSS.Article where

import           Control.Lens                      ((^.))
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B   
import qualified Data.ByteString.Lazy.Char8 as L8  
import           Data.Maybe                        (fromJust,isJust,isNothing)
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

whatConst :: SourceConstraint -> String
whatConst sc
  | (isJust (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))       = "SrcAndBetTime"
  | (isNothing (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))    = "BetweenTime"
  | (isJust (_source sc)) && (isNothing (_bTime sc)) && (isNothing (_eTime sc)) = "Source"
  | otherwise                                                                   = "Not Supported"

getRSSArticleBy :: PathConfig -> SourceConstraint -> IO [Maybe (Ar.RSSArticleH,ItemRSS)]
getRSSArticleBy cfg sc = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- case (whatConst sc) of
                "SrcAndBetTime" -> runQuery conn (queryRSSArticleBySourceAndBetTime (T.unpack $ fromJust $ _source sc) (fromJust $ _bTime sc) (fromJust $ _eTime sc))
                "BetweenTime"   -> runQuery conn (queryRSSArticleBetweenTime (fromJust $ _bTime sc) (fromJust $ _eTime sc))
                "Source"        -> getRSSArticleBySource conn (T.unpack $ fromJust $ _source sc)
                otherwise       -> return []
  result <- flip mapM articles $ \x -> do
    let hsh = L8.unpack $ L8.fromStrict $ B16.encode $ Ar._hash x
        src = T.unpack $ Ar._source x
        fileprefix = (cfg ^. rssstore) </> src
        filepath = fileprefix </> itempath {- </> (take 2 hsh) -} </> hsh
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        content <- loadItemRSS bstr
        return ((,) <$> Just x <*> content)
      False -> return Nothing -- putStrLn ("Following article exists in DB, but does not exist on disk : " ++ hsh) >> return Nothing -- error "error"
  PGS.close conn
  return result

loadItemRSS :: B.ByteString -> IO (Maybe ItemRSS)
loadItemRSS bstr = do
  let esrc = eitherDecodeStrict bstr :: Either String ItemRSS
  case esrc of
    Left  _   -> return Nothing
    Right src -> return (Just src)
