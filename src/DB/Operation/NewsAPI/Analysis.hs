module DB.Operation.NewsAPI.Analysis where

import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres (runBeamPostgresDebug,Pg)
import           Database.PostgreSQL.Simple (Connection)
--
import DB.Schema.NewsAPI
import DB.Schema.NewsAPI.Analysis



queryAnalysisAll :: Pg [Analysis]
queryAnalysisAll =
  runSelectReturningList $ select $
    all_ (_newsapiAnalyses newsAPIDB)


queryAnalysisBySource :: String -> Pg [Analysis]
queryAnalysisBySource src =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (_analysisSource a ==. val_ (T.pack src))
    pure a


queryAnalysisByTime :: UTCTime -> Pg [Analysis]
queryAnalysisByTime time =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (val_ time <=. _analysisCreated a)
    pure a


queryAnalysisBySourceAndTime :: String -> UTCTime -> Pg [Analysis]
queryAnalysisBySourceAndTime src time =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (_analysisSource a ==. val_ (T.pack src))
    guard_ (val_ time <=. _analysisCreated a)
    pure a


queryAnalysisByHash :: ByteString -> Pg [Analysis]
queryAnalysisByHash hsh =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (_analysisSHA256 a ==. val_ hsh)
    pure a


getAnalysisByHash :: ByteString -> Connection -> IO [Analysis]
getAnalysisByHash hsh conn =
  runBeamPostgresDebug putStrLn conn $ queryAnalysisByHash hsh


uploadAnalysis :: Connection -> Analysis -> IO ()
uploadAnalysis conn analysis = do
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiAnalyses newsAPIDB) $
      insertValues [ analysis ]


uploadAnalysisIfMissing :: Connection -> Analysis -> IO ()
uploadAnalysisIfMissing conn analysis = do
  as' <- getAnalysisByHash (_analysisSHA256 analysis) conn
  case as' of
    [] -> uploadAnalysis conn analysis
    as -> print "Already exists"


{-
    conn Analysis.table $
    Analysis.newAnalysis analysis_hash analysis_source analysis_corenlp analysis_srl analysis_ner analysis_created
  return ()
-}
