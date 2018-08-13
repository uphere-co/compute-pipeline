{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

module DB.Operation.RSS.Summary where

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
import DB.Schema.RSS.Summary

type EExpr = QExpr PgExpressionSyntax

type SExpr s = Q PgSelectSyntax RSSDB s (SummaryT (EExpr s))

type Condition s = SummaryT (EExpr s) -> EExpr s Bool


querySummary :: Condition s -> SExpr s
querySummary cond = do
  a <- all_ (_summaries rssDB)
  guard_ (cond a)
  pure a


countSummary :: (forall s. Condition s) -> Pg (Maybe Int)
countSummary cond =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ querySummary cond


byHash :: ByteString -> Condition s
byHash hsh a = a^.summaryHash ==. val_ hsh


publishedAfter :: UTCTime -> Condition s
publishedAfter time a = val_ time <=. a ^.summaryPubDate


publishedBefore :: UTCTime -> Condition s
publishedBefore time a = a^.summaryPubDate <=. val_ time


publishedBetween :: UTCTime -> UTCTime -> Condition s
publishedBetween time1 time2 a = publishedAfter time1 a &&. publishedBefore time2 a


uploadSummary :: Connection -> Summary -> IO ()
uploadSummary conn summary =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_summaries rssDB) $
      insertValues [summary]
