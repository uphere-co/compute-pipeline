{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.RSS.Article where

import           Control.Error.Safe                (rightMay)
import           Control.Lens                      ((^.),to)
import           Data.Aeson                        (eitherDecodeStrict)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe                        (isJust,isNothing,maybe)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock                   (UTCTime)
import           Database.Beam                     (select,runSelectReturningList,val_
                                                   ,(&&.))
import           Database.Beam.Postgres            (runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as PGS
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import           DB.Operation.RSS.Article
import           DB.Schema.RSS.Article
import           NLP.Shared.Type                   (Summary(..),PathConfig
                                                   ,dbstring,rssstore
                                                   )
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
  PGS.close conn
  let mkPair :: RSSArticle -> (Text,Text)
      mkPair x = (x^.rssArticleSource, x^.rssArticleHash.to B16.encode.to TE.decodeUtf8)
  return (map mkPair articles)

{-
whatConst :: SourceConstraint -> String
whatConst sc
  | (isJust (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))       = "SrcAndBetTime"
  | (isNothing (_source sc)) && (isJust (_bTime sc)) && (isJust (_eTime sc))    = "BetweenTime"
  | (isJust (_source sc)) && (isNothing (_bTime sc)) && (isNothing (_eTime sc)) = "Source"
  | otherwise                                                                   = "Not Supported"
-}

getRSSArticleBy :: PathConfig -> SourceTimeConstraint -> IO [Maybe (RSSArticle,Summary)]
getRSSArticleBy cfg (msrc,tc) = do
  conn <- getConnection (cfg ^. dbstring)
  let srcconst a = maybe (val_ True) (\src -> bySource src a) msrc
      timeconst a = case tc of
                      Nothing -> val_ True
                      Just (Between btime etime) -> createdBetween btime etime a
                      Just (After btime) -> createdAfter btime a
                      Just (Before etime) -> createdBefore etime a

  articles <- runBeamPostgresDebug putStrLn conn $
                runSelectReturningList $
                  select $
                    queryArticle (\a -> srcconst a &&. timeconst a)
  {-
    case (whatConst sc) of
                "SrcAndBetTime" ->
                  runBeamPostgresDebug putStrLn conn $
                    runSelectReturningList $
                      select $
                        let src = fromJust $ _source sc
                            btime = fromJust $ _bTime sc
                            etime = fromJust $ _eTime sc
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
                otherwise       -> pure [] -}
  result <- flip mapM articles $ \x -> do
    let hshtxt = x^.rssArticleHash.to B16.encode.to TE.decodeUtf8
        src = x^.rssArticleSource
        fileprefix = (cfg ^. rssstore) </> T.unpack src
        filepath = fileprefix </> itempath </> take 2 (T.unpack hshtxt) </> T.unpack hshtxt
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        bstr <- B.readFile filepath
        let content = rightMay (eitherDecodeStrict bstr)
        return ((,) <$> Just x <*> content)
      False -> return Nothing
  PGS.close conn
  return result
