module DB.Operation.NewsAPI.Article where

import           Control.Monad (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres (runBeamPostgresDebug,Pg)
import           Database.PostgreSQL.Simple (Connection)
--
import DB.Schema.NewsAPI
import DB.Schema.NewsAPI.Article


-- from newsapi


{-
queryArticleAll :: Query (To Column (A.Article))
queryArticleAll = proc () -> do
  r <- A.queryAll -< ()
  returnA -< r

queryArticleBySource :: String -> Query (To Column (A.Article))
queryArticleBySource src = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  returnA -< r

queryArticleByTime :: UTCTime -> Query (To Column (A.Article))
queryArticleByTime time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r

queryArticleBySourceAndTime :: String -> UTCTime -> Query (To Column (A.Article))
queryArticleBySourceAndTime src time = proc () -> do
  r <- A.queryAll -< ()
  restrict -< A._source r .== (constant (T.pack src))
  restrict -< pgUTCTime time .<= (safeCoerceToRep $ A._created r)
  returnA -< r





getArticleAll conn = (runQuery conn queryArticleAll :: IO [A.ArticleH])

getArticleBySource src conn = (runQuery conn (queryArticleBySource src) :: IO [A.ArticleH])

getArticleByTime time conn = (runQuery conn (queryArticleByTime time) :: IO [A.ArticleH])

getArticleBySourceAndTime conn src time = (runQuery conn (queryArticleBySourceAndTime src time) :: IO [A.ArticleH])


uploadArticle :: Connection -> Article -> IO ()
uploadArticle conn article = do
  void . runBeamPostgresDebug putStrLn conn . runInsert $
    insert (_newsapiArticles newsAPIDB) $
      insertValues [ article ]
-}
