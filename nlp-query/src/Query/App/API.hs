{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Query.App.API where

import           Control.Concurrent                (threadDelay)
import           Control.Lens                      ((^.))
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson         as A
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
import           Database.PostgreSQL.Simple        (Connection)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
--
import           NewsAPI.DB                        (getArticleBySourceAndTime)
import qualified NewsAPI.DB.Article as Ar
--

nominalDay :: NominalDiffTime
nominalDay = 86400

oneDayArticles conn = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime 
  articles <- getArticleBySourceAndTime conn "bloomberg" yesterday
  return articles

getOneDayArticles conn = do
  articles <- oneDayArticles conn
  let idList = map (Ar._id) articles
  return idList

-- * api

type RecentArticleAPI = "recentarticle" :> Get '[JSON] [RecentArticle]

recentarticleAPI :: Proxy RecentArticleAPI
recentarticleAPI = Proxy

-- * app

run :: Connection -> IO ()
run conn = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< (mkApp conn)

mkApp :: Connection -> IO Application
mkApp conn = return $ serve recentarticleAPI (server conn)

server :: Connection -> Server RecentArticleAPI
server conn = getArticles conn

getArticles :: Connection -> Handler [RecentArticle]
getArticles conn = do
  list <- liftIO $ getOneDayArticles conn
  let result = map (\x -> RecentArticle (toInteger x)) list
  return result

-- * article

data RecentArticle = RecentArticle
  { articleId :: Integer
  } deriving (Eq, Show, Generic)

instance A.ToJSON RecentArticle
instance A.FromJSON RecentArticle
