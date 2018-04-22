{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
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

type EExpr = QExpr PgExpressionSyntax 

type SExpr s = Q PgSelectSyntax NewsAPIDB s (ArticleT (EExpr s))

type Condition s = ArticleT (EExpr s) -> EExpr s Bool


queryArticle :: Condition s -> SExpr s
queryArticle cond = do
  a <- all_ (_newsapiArticles newsAPIDB)
  guard_ (cond a)
  pure a


countArticle :: (forall s. Condition s) -> Pg (Maybe Int)
countArticle cond =
  runSelectReturningOne $ select $ 
    aggregate_ (\a -> as_ @Int countAll_) $ queryArticle cond


byId :: Text -> Condition s
byId id' a = a^.articleId ==. val_ id'


byHash :: ByteString -> Condition s
byHash hsh a = a^.articleHash ==. val_ hsh


bySource :: Text -> Condition s
bySource src a = a^.articleSource ==. val_ src


createdAfter :: UTCTime -> Condition s
createdAfter time a = val_ time <=. a ^.articleCreated


createdBefore :: UTCTime -> Condition s
createdBefore time a = a^.articleCreated <=. val_ time


createdBetween :: UTCTime -> UTCTime -> Condition s
createdBetween time1 time2 a = createdAfter time1 a &&. createdBefore time2 a



uploadArticle :: Connection -> Article -> IO ()
uploadArticle conn article = do
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiArticles newsAPIDB) $
      insertValues [ article ]
