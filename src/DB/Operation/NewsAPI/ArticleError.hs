{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeApplications #-}
module DB.Operation.NewsAPI.ArticleError where

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
import DB.Schema.NewsAPI.ArticleError



uploadArticleError :: Connection -> ArticleError -> IO ()
uploadArticleError conn err = do
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiArticleErrors newsAPIDB) $
      insertValues [ err ]


