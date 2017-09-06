module Query.App.API where


import           Control.Concurrent                (threadDelay)
import           Control.Lens                      ((^.))
import           Control.Monad                     (forever)
import qualified Data.Aeson         as A
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
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

getOneDayArticles conn = forever $ do
  articles <- oneDayArticles conn
  print $ map (Ar._id) articles
  threadDelay 10000000
