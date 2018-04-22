{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}
module DB.Operation.RSS.Analysis where

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
import DB.Schema.RSS.Analysis

type EExpr = QExpr PgExpressionSyntax 

type SExpr s = Q PgSelectSyntax RSSDB s (RSSAnalysisT (EExpr s))

type Condition s = RSSAnalysisT (EExpr s) -> EExpr s Bool

queryAnalysis :: Condition s -> SExpr s
queryAnalysis cond = do
  a <- all_ (_rssAnalyses rssDB)
  guard_ (cond a)
  pure a

countAnalysis :: (forall s. Condition s) -> Pg (Maybe Int)
countAnalysis cond =
  runSelectReturningOne $ select $ 
    aggregate_ (\a -> as_ @Int countAll_) $ queryAnalysis cond
  

bySource :: Text -> Condition s
bySource src a = a^.rssAnalysisSource ==. val_ src


byHash :: ByteString -> Condition s
byHash hsh a = a^.rssAnalysisHash ==. val_ hsh


createdAfter :: UTCTime -> Condition s
createdAfter time a = val_ time <=. a ^.rssAnalysisCreated


createdBefore :: UTCTime -> Condition s
createdBefore time a = a^.rssAnalysisCreated <=. val_ time


createdBetween :: UTCTime -> UTCTime -> Condition s
createdBetween time1 time2 a = createdAfter time1 a &&. createdBefore time2 a


uploadRSSAnalysis :: Connection -> RSSAnalysis -> IO ()
uploadRSSAnalysis conn analysis =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_rssAnalyses rssDB) $
      insertValues [analysis]


uploadRSSAnalysisIfMissing :: Connection -> RSSAnalysis -> IO ()
uploadRSSAnalysisIfMissing conn analysis = do
  as' <- runBeamPostgresDebug putStrLn conn $
           runSelectReturningList $
             select $
               queryAnalysis (byHash (analysis^.rssAnalysisHash))
  case as' of
    []  -> uploadRSSAnalysis conn analysis
    _as -> putStrLn "Already exists"



updateRSSAnalysisStatus :: Connection
                        -> ByteString
                        -> (Maybe Bool,Maybe Bool,Maybe Bool)
                        -> IO ()
updateRSSAnalysisStatus conn hsh (mb1,mb2,mb3) =
  runBeamPostgresDebug putStrLn conn $
    runUpdate $
      update (_rssAnalyses rssDB)
             (\analysis -> [ analysis^.rssAnalysisCoreNLP <-. val_ mb1
                           , analysis^.rssAnalysisSRL     <-. val_ mb2
                           , analysis^.rssAnalysisNER     <-. val_ mb3
                           ]
              )
             (\analysis -> analysis ^.rssAnalysisHash ==. (val_ hsh))
