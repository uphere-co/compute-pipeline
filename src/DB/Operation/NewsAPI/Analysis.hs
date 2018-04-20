module DB.Operation.NewsAPI.Analysis where

import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres (runBeamPostgresDebug,Pg)
import           Database.PostgreSQL.Simple (Connection)
import           Lens.Micro
--
import DB.Schema.NewsAPI
import DB.Schema.NewsAPI.Analysis



queryAnalysisAll :: Pg [Analysis]
queryAnalysisAll =
  runSelectReturningList $ select $
    all_ (_newsapiAnalyses newsAPIDB)

queryAnalysisByHash :: ByteString -> Pg [Analysis]
queryAnalysisByHash hsh =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (a^.analysisSHA256 ==. val_ hsh)
    pure a

queryAnalysisBySource :: String -> Pg [Analysis]
queryAnalysisBySource src =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (a^.analysisSource ==. val_ (T.pack src))
    pure a


queryAnalysisByTime :: UTCTime -> Pg [Analysis]
queryAnalysisByTime time =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (val_ time <=. a^.analysisCreated)
    pure a


queryAnalysisBySourceAndTime :: String -> UTCTime -> Pg [Analysis]
queryAnalysisBySourceAndTime src time =
  runSelectReturningList $ select $ do
    a <- all_ (_newsapiAnalyses newsAPIDB)
    guard_ (_analysisSource a ==. val_ (T.pack src))
    guard_ (val_ time <=. a^.analysisCreated)
    pure a




getAnalysisAll :: Connection -> IO [Analysis]
getAnalysisAll conn =
  runBeamPostgresDebug putStrLn conn queryAnalysisAll

getAnalysisByHash :: Connection -> ByteString -> IO [Analysis]
getAnalysisByHash conn hsh =
  runBeamPostgresDebug putStrLn conn $ queryAnalysisByHash hsh


getAnalysisBySource :: Connection -> String -> IO [Analysis]
getAnalysisBySource conn src =
  runBeamPostgresDebug putStrLn conn $ queryAnalysisBySource src


getAnalysisByTime :: Connection -> UTCTime -> IO [Analysis]
getAnalysisByTime conn time =
  runBeamPostgresDebug putStrLn conn $ queryAnalysisByTime time


getAnalysisBySourceAndTime :: Connection -> String -> UTCTime -> IO [Analysis]
getAnalysisBySourceAndTime conn src time =
  runBeamPostgresDebug putStrLn conn $ queryAnalysisBySourceAndTime src time


uploadAnalysis :: Connection -> Analysis -> IO ()
uploadAnalysis conn analysis =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiAnalyses newsAPIDB) $
      insertValues [ analysis ]


uploadAnalysisIfMissing :: Connection -> Analysis -> IO ()
uploadAnalysisIfMissing conn analysis = do
  as' <- getAnalysisByHash conn (_analysisSHA256 analysis)
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
