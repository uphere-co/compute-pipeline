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
import           Database.Beam                     (select,runSelectReturningList
                                                   ,exists_,filter_,guard_,not_
                                                   ,val_,all_
                                                   ,(&&.),(==.),(/=.))
import           Database.Beam.Postgres            (runBeamPostgresDebug)
import qualified Database.PostgreSQL.Simple as PGS
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import qualified DB.Operation.RSS.Analysis  as Analysis
import qualified DB.Operation.RSS.Article   as Article
import qualified DB.Operation.RSS.Summary   as Summary
-- import           DB.Schema.RSS.Analysis            (rssAnalysisHash,rssAnalysisCoreNLP)
import           DB.Schema.RSS                     (rssDB,_coreNLPs,_summaries)
import           DB.Schema.RSS.Article             (RSSArticle
                                                   ,rssArticleSource
                                                   ,rssArticleHash,rssArticleHash)
import           DB.Schema.RSS.CoreNLP
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


srcconst msrc a = maybe (val_ True) (\src -> Article.bySource src a) msrc
timeconst tc a =
  case tc of
    Nothing -> val_ True
    Just (Between btime etime) -> Article.createdBetween btime etime a
    Just (After btime) -> Article.createdAfter btime a
    Just (Before etime) -> Article.createdBefore etime a


-- | list new articles satisfying constraint, and not parsed yet.
--
listNewArticles :: PathConfig
                -> SourceTimeConstraint
                -> IO [(RSSArticle,NLP.Shared.Type.Summary)]
listNewArticles cfg (msrc,tc) = do
  conn <- getConnection (cfg ^. dbstring)

  articles :: [(RSSArticle,Summary)] <-
    runBeamPostgresDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          a <- Article.queryArticle (\a -> srcconst msrc a &&. timeconst tc a)
          let hsh = a^.rssArticleHash
          s <- Summary.querySummary (\s -> (s^.summaryHash ==. hsh))
          guard_ . not_ . exists_ . filter_ (\c -> c^.coreNLPHash ==. hsh) $
            (all_ (_coreNLPs rssDB))
          pure (a,s)
  PGS.close conn
  (return . map (_2 %~ toSummary)) articles

toSummary :: Summary -> NLP.Shared.Type.Summary
toSummary s = NLP.Shared.Type.Summary
                (s^.summaryLink)
                (s^.summaryTitle)
                (s^.summaryDescription)
                (s^.summaryPubDate)
