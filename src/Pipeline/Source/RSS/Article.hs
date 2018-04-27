{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Source.RSS.Article where

import           Control.Error.Safe                (rightMay)
import           Control.Lens                      ((^.),(%~),to,_2)
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
                                                   ,(&&.),(==.))
import           Database.Beam.Postgres            (runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as PGS
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import qualified DB.Operation.RSS.Article   as Article
import qualified DB.Operation.RSS.Summary   as Summary
import           DB.Schema.RSS.Article             (RSSArticle
                                                   ,rssArticleSource
                                                   ,rssArticleHash,rssArticleHash)
import           DB.Schema.RSS.Summary             (Summary,summaryHash
                                                   ,summaryLink,summaryTitle
                                                   ,summaryDescription,summaryPubDate
                                                   )
import           NLP.Shared.Type                   (-- Summary(..),
                                                    PathConfig
                                                   ,dbstring,rssstore
                                                   )
import qualified NLP.Shared.Type                   (Summary(..))
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
                    Article.queryArticle (Article.createdAfter time)
  PGS.close conn
  let mkPair :: RSSArticle -> (Text,Text)
      mkPair x = (x^.rssArticleSource, x^.rssArticleHash.to B16.encode.to TE.decodeUtf8)
  return (map mkPair articles)

getRSSArticleBy :: PathConfig
                -> SourceTimeConstraint
                -> IO [(RSSArticle,NLP.Shared.Type.Summary)]
getRSSArticleBy cfg (msrc,tc) = do
  conn <- getConnection (cfg ^. dbstring)
  let srcconst a = maybe (val_ True) (\src -> Article.bySource src a) msrc
      timeconst a = case tc of
                      Nothing -> val_ True
                      Just (Between btime etime) -> Article.createdBetween btime etime a
                      Just (After btime) -> Article.createdAfter btime a
                      Just (Before etime) -> Article.createdBefore etime a

  articles :: [(RSSArticle,Summary)]
    <- runBeamPostgresDebug putStrLn conn $
         runSelectReturningList $
           select $ do
             a <- Article.queryArticle (\a -> srcconst a &&. timeconst a)
             -- let hsh = a^.rssArticleHash
             s <- Summary.querySummary (\s -> s^.summaryHash ==. a^.rssArticleHash)
             return (a,s)
  -- print (take 10 articles)
 {-                     
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
  -}
  PGS.close conn
  -- let result = [] 
  (return . map (_2 %~ toSummary)) articles -- result

toSummary :: Summary -> NLP.Shared.Type.Summary
toSummary s = NLP.Shared.Type.Summary
                (s^.summaryLink)
                (s^.summaryTitle)
                (s^.summaryDescription)
                (s^.summaryPubDate)
