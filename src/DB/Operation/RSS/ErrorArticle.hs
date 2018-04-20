{-# LANGUAGE TypeApplications #-}
module DB.Operation.RSS.ErrorArticle where

import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres (runBeamPostgresDebug,Pg)
import           Database.PostgreSQL.Simple (Connection)
import           Lens.Micro
--
import DB.Schema.RSS
import DB.Schema.RSS.ErrorArticle


queryRSSErrorArticleByHash :: ByteString -> Pg [RSSErrorArticle]
queryRSSErrorArticleByHash hsh =
  runSelectReturningList $ select $ do
    a <- all_ (_rssErrorArticles rssDB)
    guard_ (a^.rssErrorHash ==. val_ hsh)
    pure a


uploadRSSErrorArticle :: Connection -> RSSErrorArticle -> IO ()
uploadRSSErrorArticle conn err =
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_rssErrorArticles rssDB) $
      insertValues [err]


uploadRSSErrorArticleIfMissing :: Connection -> RSSErrorArticle -> IO ()
uploadRSSErrorArticleIfMissing conn err = do
  as' <- runBeamPostgresDebug putStrLn conn (queryRSSErrorArticleByHash (err^.rssErrorHash))
  case as' of
    []  -> uploadRSSErrorArticle conn err
    _as -> putStrLn "Already exists"
