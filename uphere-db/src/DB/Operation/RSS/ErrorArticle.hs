{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}
module DB.Operation.RSS.ErrorArticle where

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
import DB.Schema.RSS.ErrorArticle


type EExpr = QExpr PgExpressionSyntax

type SExpr s = Q PgSelectSyntax RSSDB s (RSSErrorArticleT (EExpr s))

type Condition s = RSSErrorArticleT (EExpr s) -> EExpr s Bool

queryErrorArticle :: Condition s -> SExpr s
queryErrorArticle cond = do
  a <- all_ (_rssErrorArticles rssDB)
  guard_ (cond a)
  pure a

countErrorArticle :: (forall s. Condition s) -> Pg (Maybe Int)
countErrorArticle cond =
  runSelectReturningOne $ select $
    aggregate_ (\a -> as_ @Int countAll_) $ queryErrorArticle cond


byHash :: ByteString -> Condition s
byHash hsh a = a^.rssErrorHash ==. val_ hsh



uploadRSSErrorArticle :: Connection -> RSSErrorArticle -> IO ()
uploadRSSErrorArticle conn err =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_rssErrorArticles rssDB) $
      insertValues [err]


uploadRSSErrorArticleIfMissing :: Connection -> RSSErrorArticle -> IO ()
uploadRSSErrorArticleIfMissing conn err = do
  as' <- runBeamPostgresDebug putStrLn conn $
           runSelectReturningList $
             select $
               queryErrorArticle (byHash (err^.rssErrorHash))
  case as' of
    []  -> uploadRSSErrorArticle conn err
    _as -> putStrLn "Already exists"
