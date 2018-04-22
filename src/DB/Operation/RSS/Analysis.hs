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

countAnalyses :: (forall s. Condition s) -> Pg (Maybe Int)
countAnalyses cond =
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


{-
queryRSSAnalysisBetweenTime :: UTCTime -> UTCTime -> Pg [RSSAnalysis]
queryRSSAnalysisBetweenTime time1 time2 =
  runSelectReturningList $ select $ do
    a <- all_ (_rssAnalyses rssDB)
    guard_ (val_ time1 <=. a^.rssAnalysisCreated &&. a^.rssAnalysisCreated <=. val_ time2)
    pure a


queryRSSAnalysisBySource :: Text -> Pg [RSSAnalysis]
queryRSSAnalysisBySource src =
  runSelectReturningList $ select $ do
    a <- all_ (_rssAnalyses rssDB)
    guard_ (a^.rssAnalysisSource ==. val_ src)
    pure a


queryRSSAnalysisByTime :: UTCTime -> Pg [RSSAnalysis]
queryRSSAnalysisByTime time =
  runSelectReturningList $ select $ do
    a <- all_ (_rssAnalyses rssDB)
    guard_ (val_ time <=. a^.rssAnalysisCreated)
    pure a


queryRSSAnalysisBySourceAndTime :: Text -> UTCTime -> Pg [RSSAnalysis]
queryRSSAnalysisBySourceAndTime src time =
  runSelectReturningList $ select $ do
    a <- all_ (_rssAnalyses rssDB)
    guard_ (a^.rssAnalysisSource ==. val_ src)
    guard_ (val_ time <=. a^.rssAnalysisCreated)
    pure a


queryRSSAnalysisByHash :: ByteString -> Pg [RSSAnalysis]
queryRSSAnalysisByHash hsh =
  runSelectReturningList $ select $ do
    a <- all_ (_rssAnalyses rssDB)
    guard_ (a^.rssAnalysisHash ==. val_ hsh)
    pure a
-}


countRSSAnalysisAll :: Pg (Maybe Int)
countRSSAnalysisAll =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $
      all_ (_rssAnalyses rssDB)


countRSSAnalysisByTime :: UTCTime -> Pg (Maybe Int)
countRSSAnalysisByTime time =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ do
      a <- all_ (_rssAnalyses rssDB)
      guard_ (val_ time <=. a^.rssAnalysisCreated)
      pure a


countRSSAnalysisBetweenTime :: UTCTime -> UTCTime -> Pg (Maybe Int)
countRSSAnalysisBetweenTime time1 time2 =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ do
      a <- all_ (_rssAnalyses rssDB)
      guard_ (val_ time1 <=. a^.rssAnalysisCreated &&. a^.rssAnalysisCreated <=. val_ time2)
      pure a

{-
getRSSAnalysisAll :: Connection -> IO [RSSAnalysis]
getRSSAnalysisAll conn =
  runBeamPostgresDebug putStrLn conn queryRSSAnalysisAll

getRSSAnalysisBySource :: Connection -> Text -> IO [RSSAnalysis]
getRSSAnalysisBySource conn src =
  runBeamPostgresDebug putStrLn conn (queryRSSAnalysisBySource src)


getRSSAnalysisByTime :: Connection -> UTCTime -> IO [RSSAnalysis]
getRSSAnalysisByTime conn time =
  runBeamPostgresDebug putStrLn conn (queryRSSAnalysisByTime time)

getRSSAnalysisBySourceAndTime :: Connection -> Text -> UTCTime -> IO [RSSAnalysis]
getRSSAnalysisBySourceAndTime conn src time =
  runBeamPostgresDebug putStrLn conn (queryRSSAnalysisBySourceAndTime src time)

getRSSAnalysisByHash :: Connection -> ByteString -> IO [RSSAnalysis]
getRSSAnalysisByHash conn hsh =
  runBeamPostgresDebug putStrLn conn (queryRSSAnalysisByHash hsh)


getCountRSSAnalysisAll :: (MonadIO m) => Connection -> m Int
getCountRSSAnalysisAll conn = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn countRSSAnalysisAll
  return n


getCountRSSAnalysisByTime :: (MonadIO m) => Connection -> UTCTime -> m Int
getCountRSSAnalysisByTime conn time = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn (countRSSAnalysisByTime time)
  return n


getCountRSSAnalysisBetweenTime :: Connection -> UTCTime -> UTCTime -> IO Int
getCountRSSAnalysisBetweenTime conn time1 time2 = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn (countRSSAnalysisBetweenTime time1 time2)
  return n
-}

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
