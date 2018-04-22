{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
module DB.Operation.RSS.Article where

import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.PostgreSQL.Simple (Connection)
import           Lens.Micro
--
import DB.Schema.RSS
import DB.Schema.RSS.Article

type EExpr = QExpr PgExpressionSyntax 

type SExpr s = Q PgSelectSyntax RSSDB s (RSSArticleT (EExpr s))

type Condition s = RSSArticleT (EExpr s) -> EExpr s Bool

queryArticle :: Condition s -> SExpr s
queryArticle cond = do
  a <- all_ (_rssArticles rssDB)
  guard_ (cond a)
  pure a


countArticle :: (forall s. Condition s) -> Pg (Maybe Int)
countArticle cond =
  runSelectReturningOne $ select $ 
    aggregate_ (\a -> as_ @Int countAll_) $ queryArticle cond


bySource :: Text -> Condition s
bySource src a = a^.rssArticleSource ==. val_ src


byHash :: ByteString -> Condition s
byHash hsh a = a^.rssArticleHash ==. val_ hsh


createdAfter :: UTCTime -> Condition s
createdAfter time a = val_ time <=. a ^.rssArticleCreated


createdBefore :: UTCTime -> Condition s
createdBefore time a = a^.rssArticleCreated <=. val_ time


createdBetween :: UTCTime -> UTCTime -> Condition s
createdBetween time1 time2 a = createdAfter time1 a &&. createdBefore time2 a


uploadRSSArticle :: Connection -> RSSArticle -> IO ()
uploadRSSArticle conn article =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_rssArticles rssDB) $
      insertValues [article] 


uploadRSSArticleIfMissing :: Connection -> RSSArticle -> IO ()
uploadRSSArticleIfMissing conn article = do
  as' <- runBeamPostgresDebug putStrLn conn $
           runSelectReturningList $
             select $
               queryArticle (byHash (article^.rssArticleHash))
  case as' of
    []  -> uploadRSSArticle conn article
    _as -> putStrLn "Already exists"

