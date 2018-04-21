module DB.Operation.NewsAPI.Article where

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
import DB.Schema.NewsAPI.Article

type SelectExpr s = Q PgSelectSyntax NewsAPIDB s (ArticleT (QExpr PgExpressionSyntax s))

queryArticleAll :: SelectExpr s 
queryArticleAll = all_ (_newsapiArticles newsAPIDB)


queryArticleBySource :: Text -> SelectExpr s
queryArticleBySource src = do
  a <- all_ (_newsapiArticles newsAPIDB)
  guard_ (a^.articleSource ==. val_ src)
  pure a

queryArticleByTime :: UTCTime -> SelectExpr s
queryArticleByTime time = do
  a <- all_ (_newsapiArticles newsAPIDB)
  guard_ (val_ time <=. a^.articleCreated)
  pure a


queryArticleBySourceAndTime :: Text -> UTCTime -> SelectExpr s
queryArticleBySourceAndTime src time = do
  a <- all_ (_newsapiArticles newsAPIDB)
  guard_ (a^.articleSource ==. val_ src)
  guard_ (val_ time <=. a^.articleCreated)
  return a

{- 
getArticleAll :: Connection -> IO [Article]
getArticleAll conn =
  runBeamPostgresDebug putStrLn conn (runSelectReturningList $ select queryArticleAll)


getArticleBySource :: Connection -> Text -> IO [Article]
getArticleBySource conn src =
  runBeamPostgresDebug putStrLn conn (runSelectReturningList $ select (queryArticleBySource src))


getArticleByTime :: Connection -> UTCTime -> IO [Article]
getArticleByTime conn time =
  runBeamPostgresDebug putStrLn conn (queryArticleByTime time)


getArticleBySourceAndTime :: Connection -> Text -> UTCTime -> IO [Article]
getArticleBySourceAndTime conn src time =
  runBeamPostgresDebug putStrLn conn (queryArticleBySourceAndTime src time)
-}

uploadArticle :: Connection -> Article -> IO ()
uploadArticle conn article = do
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiArticles newsAPIDB) $
      insertValues [ article ]
