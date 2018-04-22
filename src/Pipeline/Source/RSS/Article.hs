{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.RSS.Article where

import           Control.Lens                      ((^.),to)
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe                        (fromJust,isJust,isNothing)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock                   (UTCTime)
import           Database.Beam                     (select,runSelectReturningList,(&&.))
import           Database.Beam.Postgres            (runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as PGS
-- import           Opaleye                           (runQuery)
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
-- import           DB.Operation.RSS.Analysis
import           DB.Operation.RSS.Article
-- import           DB.Operation.RSS.ErrorArticle
import           DB.Schema.RSS.Article
import           NLP.Shared.Type
-- import           RSS.Type                          (itempath)
--
import           Pipeline.Operation.DB
import           Pipeline.Type


itempath :: FilePath
itempath = "RSSItem"


getHashByTime :: PathConfig -> UTCTime -> IO [(Text,Text)]
getHashByTime cfg time = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- runBeamPostgresDebug putStrLn conn $
                runSelectReturningList $
                  select $
                    queryArticle (createdAfter time)

  --   getRSSArticleByTime conn time
  PGS.close conn
  let mkPair :: RSSArticle -> (Text,Text)
      mkPair x = (x^.rssArticleSource, x^.rssArticleHash.to B16.encode.to TE.decodeUtf8)
  return (map mkPair articles)

whatConst :: SourceConstraint -> String
whatConst sc
  | (isJust (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))       = "SrcAndBetTime"
  | (isNothing (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))    = "BetweenTime"
  | (isJust (_source sc)) && (isNothing (_bTime sc)) && (isNothing (_eTime sc)) = "Source"
  | otherwise                                                                   = "Not Supported"

getRSSArticleBy :: PathConfig -> SourceConstraint -> IO [Maybe (RSSArticle,ItemRSS)]
getRSSArticleBy cfg sc = do
  conn <- getConnection (cfg ^. dbstring)
  articles <- case (whatConst sc) of
                "SrcAndBetTime" ->
                  runBeamPostgresDebug putStrLn conn $
                    runSelectReturningList $
                      select $
                        let src = fromJust $ _source sc
                            btime = fromJust $ _bTime sc
                            etime = fromJust $ _eTime sc
                        in queryArticle (\a ->     bySource src a
                                               &&. createdBetween btime etime a)
                "BetweenTime"   ->
                  runBeamPostgresDebug putStrLn conn $
                    runSelectReturningList $
                      select $
                        let btime = fromJust $ _bTime sc
                            etime = fromJust $ _eTime sc
                        in queryArticle (createdBetween btime etime)
                "Source"        ->
                  runBeamPostgresDebug putStrLn conn $
                    runSelectReturningList $
                      select $
                        queryArticle (bySource (fromJust (_source sc)))
                otherwise       -> pure []
  result <- flip mapM articles $ \x -> do
    let hshtxt = x^.rssArticleHash.to B16.encode.to TE.decodeUtf8
        src = x^.rssArticleSource
        fileprefix = (cfg ^. rssstore) </> T.unpack src
        filepath = fileprefix </> itempath </> take 2 (T.unpack hshtxt) </> T.unpack hshtxt
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
