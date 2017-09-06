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
import qualified Data.Text          as T
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
import           Pipeline.Util                     (bstrHashToB16)


nominalDay :: NominalDiffTime
nominalDay = 86400

oneDayArticles conn txt = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime 
  articles <- getArticleBySourceAndTime conn (T.unpack txt) yesterday
  return articles

getOneDayArticles conn txt = do
  articles <- oneDayArticles conn txt
  let idList = map (\x -> (Ar._id x, T.pack $ bstrHashToB16 $ Ar._sha256 x, Ar._source x)) articles
  return idList

type RecentArticleAPI = "recentarticle" :> Capture "ASource" T.Text :> Get '[JSON] [RecentArticle]

recentarticleAPI :: Proxy RecentArticleAPI
recentarticleAPI = Proxy

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
server conn = getArticlesBySrc conn

getArticlesBySrc :: Connection -> T.Text -> Handler [RecentArticle]
getArticlesBySrc conn txt = do
  list <- liftIO $ getOneDayArticles conn txt
  let result = map (\(i,hsh,src) -> RecentArticle (toInteger i) hsh src) list
  return result

data RecentArticle = RecentArticle
  { articleId :: Integer
  , articleHash :: T.Text
  , articleSource :: T.Text
  } deriving (Eq, Show, Generic)

instance A.ToJSON RecentArticle
instance A.FromJSON RecentArticle
