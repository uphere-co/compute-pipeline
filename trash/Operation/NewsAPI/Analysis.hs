{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}
module DB.Operation.NewsAPI.Analysis where

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
import DB.Schema.NewsAPI
import DB.Schema.NewsAPI.Analysis

type EExpr = QExpr PgExpressionSyntax

type SExpr s = Q PgSelectSyntax NewsAPIDB s (AnalysisT (EExpr s))

type Condition s = AnalysisT (EExpr s) -> EExpr s Bool

queryAnalysis :: Condition s -> SExpr s
queryAnalysis cond = do
  a <- all_ (_newsapiAnalyses newsAPIDB)
  guard_ (cond a)
  pure a

countAnalysis :: (forall s. Condition s) -> Pg (Maybe Int)
countAnalysis cond =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ queryAnalysis cond


bySource :: Text -> Condition s
bySource src a = a^.analysisSource ==. val_ src


bySHA256 :: ByteString -> Condition s
bySHA256 hsh a = a^.analysisSHA256 ==. val_ hsh


createdAfter :: UTCTime -> Condition s
createdAfter time a = val_ time <=. a ^.analysisCreated


createdBefore :: UTCTime -> Condition s
createdBefore time a = a^.analysisCreated <=. val_ time


createdBetween :: UTCTime -> UTCTime -> Condition s
createdBetween time1 time2 a = createdAfter time1 a &&. createdBefore time2 a




uploadAnalysis :: Connection -> Analysis -> IO ()
uploadAnalysis conn analysis =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiAnalyses newsAPIDB) $
      insertValues [ analysis ]


uploadAnalysisIfMissing :: Connection -> Analysis -> IO ()
uploadAnalysisIfMissing conn analysis = do
  as' <- runBeamPostgresDebug putStrLn conn $
           runSelectReturningList $
             select $
               queryAnalysis (bySHA256 (analysis^.analysisSHA256))
  case as' of
    [] -> uploadAnalysis conn analysis
    as -> print "Already exists"


  -- What happens if ov is null? I couldn't find a way to check if it is null.
  -- Due to the test, if ov is null, then null is inserted despite outer Just.
updateAnalysisStatus :: Connection
                     -> ByteString
                     -> (Maybe Bool,Maybe Bool,Maybe Bool)
                     -> IO ()
updateAnalysisStatus conn hsh (mb1,mb2,mb3) =
  runBeamPostgresDebug putStrLn conn $
    runUpdate $
      update (_newsapiAnalyses newsAPIDB)
             (\analysis -> [ analysis^.analysisCoreNLP <-. val_ mb1
                           , analysis^.analysisSRL     <-. val_ mb2
                           , analysis^.analysisNER     <-. val_ mb3
                           ]
              )
             (\analysis -> analysis ^.analysisSHA256 ==. (val_ hsh))
