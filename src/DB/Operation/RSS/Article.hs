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

type SelectExpr s = Q PgSelectSyntax RSSDB s (RSSArticleT (QExpr PgExpressionSyntax s))


queryRSSArticleAll :: SelectExpr s
queryRSSArticleAll = all_ (_rssArticles rssDB)

queryRSSArticleBetweenTime :: UTCTime -> UTCTime -> SelectExpr s
queryRSSArticleBetweenTime time1 time2 = do
  a <- all_ (_rssArticles rssDB)
  guard_ ((val_ time1 <=. a^.rssArticleCreated) &&.
          (a^.rssArticleCreated <=. val_ time2))
  pure a

queryRSSArticleBySource :: Text -> SelectExpr s
queryRSSArticleBySource src = do
  a <- all_ (_rssArticles rssDB)
  guard_ (a^.rssArticleSource ==. val_ src)
  pure a

queryRSSArticleByTime :: UTCTime -> SelectExpr s
queryRSSArticleByTime time = do
  a <- all_ (_rssArticles rssDB)
  guard_ (val_ time <=. a^.rssArticleCreated)
  pure a


queryRSSArticleBySourceAndTime :: Text -> UTCTime -> SelectExpr s
queryRSSArticleBySourceAndTime src time = do
  a <- all_ (_rssArticles rssDB)
  guard_ (a^.rssArticleSource ==. val_ src)
  guard_ (val_ time <=. a^.rssArticleCreated)
  pure a

queryRSSArticleBySourceAndBetTime :: Text -> UTCTime -> UTCTime -> SelectExpr s
queryRSSArticleBySourceAndBetTime src time1 time2 = do
  a <- all_ (_rssArticles rssDB)
  guard_ (a^.rssArticleSource ==. val_ src)
  guard_ ((val_ time1 <=. a^.rssArticleCreated) &&. (a^.rssArticleCreated <=. val_ time2))
  pure a


queryRSSArticleByHash :: ByteString -> SelectExpr s
queryRSSArticleByHash hsh = do
  a <- all_ (_rssArticles rssDB)
  guard_ (a^.rssArticleHash ==. val_ hsh)
  pure a 


countRSSArticleAll :: Pg (Maybe Int)
countRSSArticleAll =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $
      all_ (_rssArticles rssDB)


countRSSArticleByTime :: UTCTime -> Pg (Maybe Int)
countRSSArticleByTime time =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ do
      a <- all_ (_rssArticles rssDB)
      guard_ (val_ time <=. a^.rssArticleCreated)
      pure a      

countRSSArticleBetweenTime :: UTCTime -> UTCTime -> Pg (Maybe Int)
countRSSArticleBetweenTime time1 time2 =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ do
      a <- all_ (_rssArticles rssDB)
      guard_ ((val_ time1 <=. a^.rssArticleCreated) &&.
              (a^.rssArticleCreated <=. val_ time2))
      pure a

{- 
getRSSArticleAll :: Connection -> IO [RSSArticle]
getRSSArticleAll conn =
  runBeamPostgresDebug putStrLn conn queryRSSArticleAll


getRSSArticleBySource :: Connection -> Text -> IO [RSSArticle]
getRSSArticleBySource conn src =
  runBeamPostgresDebug putStrLn conn (queryRSSArticleBySource src)

getRSSArticleByTime :: Connection -> UTCTime -> IO [RSSArticle]
getRSSArticleByTime conn time =
  runBeamPostgresDebug putStrLn conn (queryRSSArticleByTime time)

getRSSArticleBySourceAndTime :: Connection -> Text -> UTCTime -> IO [RSSArticle]
getRSSArticleBySourceAndTime conn src time =
  runBeamPostgresDebug putStrLn conn (queryRSSArticleBySourceAndTime src time)

getRSSArticleByHash :: Connection -> ByteString -> IO [RSSArticle]
getRSSArticleByHash conn hsh =
  runBeamPostgresDebug putStrLn conn (queryRSSArticleByHash hsh)

getCountRSSArticleAll :: (MonadIO m) => Connection -> m Int
getCountRSSArticleAll conn = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn countRSSArticleAll
  return n

getCountRSSArticleByTime :: (MonadIO m) => Connection -> UTCTime -> m Int
getCountRSSArticleByTime conn time = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn (countRSSArticleByTime time)
  return n

getCountRSSArticleBetweenTime :: (MonadIO m) => Connection -> UTCTime -> UTCTime -> m Int
getCountRSSArticleBetweenTime conn time1 time2 = do
  Just n <- liftIO $ runBeamPostgresDebug putStrLn conn (countRSSArticleBetweenTime time1 time2)
  return n
-}

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
               queryRSSArticleByHash (article^.rssArticleHash)
  case as' of
    []  -> uploadRSSArticle conn article
    _as -> putStrLn "Already exists"

