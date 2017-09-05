module Query.App.API where


import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (forever)
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
--
import           NewsAPI.DB                        (getArticleByTime)
--


nominalDay :: NominalDiffTime
nominalDay = 86400

oneDayArticles conn = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime 
  articles <- getArticleByTime yesterday conn
  return articles

getOneDayArticles conn = forever $ do
  oneDayArticles conn >>= print
  threadDelay 10000000
