{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Query.App.API where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens                      ((^.))
import           Control.Monad                     (forever,forM,void,when)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                        (catMaybes)
import qualified Data.Text                  as T
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
import           Database.PostgreSQL.Simple        (Connection)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.IO
--
import           NewsAPI.DB                        (getAnalysisBySourceAndTime,getArticleBySourceAndTime)
import qualified NewsAPI.DB.Analysis        as An
import qualified NewsAPI.DB.Article         as Ar
import           NLP.Shared.Type                   (RecentAnalysis(..),RecentArticle(..))
--
import           Pipeline.Load
import           Pipeline.Run.SRL                  (ARBText)
import           Pipeline.Type
import           Pipeline.Util                     (bstrHashToB16)


updateARB savepath arbs = do
  forever $ do
    arbs' <- atomically (takeTMVar arbs)
    let cfps = map fst arbs'
    fps <- getFileListRecursively savepath
    let newarbs' = filter (\x -> not $ x `elem` cfps) fps

    newarbs'' <- forM newarbs' $ \fp -> do
      bstr <- B8.readFile fp
      return $ (,) <$> Just fp <*> A.decode (BL.fromStrict bstr)
    let newarbs = catMaybes newarbs''
    print newarbs
    if (not $ null newarbs)
      then atomically (putTMVar arbs (arbs' ++ newarbs))
      else atomically (putTMVar arbs arbs')
    threadDelay 3000000

loadExistingARB :: FilePath -> IO [Maybe (FilePath,(UTCTime,[[ARBText]]))]
loadExistingARB savepath = do
  fps <- getFileListRecursively savepath
  forM fps $ \fp -> do
    bstr <- B8.readFile fp
    return $ (,) <$> Just fp <*> A.decode (BL.fromStrict bstr)

oneDayArticles conn txt = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime 
  articles <- getArticleBySourceAndTime conn (T.unpack txt) yesterday
  return articles

getOneDayArticles conn txt = do
  articles <- oneDayArticles conn txt
  let aList = map (\x -> (Ar._id x, T.pack $ bstrHashToB16 $ Ar._sha256 x, Ar._source x)) articles
  return aList

nDayAnalyses conn txt n = do
  ctime <- getCurrentTime
  let nBeforeDays = addUTCTime (-(nominalDay * n)) ctime
  analyses <- getAnalysisBySourceAndTime conn (T.unpack txt) nBeforeDays
  return analyses

getNDayAnalyses conn txt n = do
  analyses <- nDayAnalyses conn txt n
  let anList = map (\x -> (T.pack $ bstrHashToB16 $ An._sha256 x, An._source x, An._corenlp x, An._srl x, An._ner x)) analyses 
  return anList
  
type API =    "recentarticle" :> Capture "ASource" T.Text :> Get '[JSON] [RecentArticle]
         :<|> "recentanalysis" :> Capture "AnSource" T.Text :> Get '[JSON] [RecentAnalysis]
         :<|> "recentarb" :> Get '[JSON] [[[ARBText]]]

recentarticleAPI :: Proxy API
recentarticleAPI = Proxy

run :: Connection -> IO ()
run conn = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings

  arbs <- newEmptyTMVarIO
  exstarbs <- loadExistingARB "/home/modori/temp/arb"
  print exstarbs
  atomically (putTMVar arbs (catMaybes exstarbs))
  void $ forkIO $ updateARB "/home/modori/temp/arb" arbs
  runSettings settings =<< (mkApp conn arbs)

mkApp :: Connection -> TMVar [(FilePath, (UTCTime, [[ARBText]]))] -> IO Application
mkApp conn arbs = return $ simpleCors (serve recentarticleAPI (server conn arbs))

server :: Connection -> TMVar [(FilePath, (UTCTime, [[ARBText]]))] -> Server API
server conn arbs = (getArticlesBySrc conn) :<|> (getAnalysesBySrc conn) :<|> (getARB arbs)

getArticlesBySrc :: Connection -> T.Text -> Handler [RecentArticle]
getArticlesBySrc conn txt = do
  list <- liftIO $ getOneDayArticles conn txt
  let result = map (\(i,hsh,src) -> RecentArticle i hsh src) list
  return result

getAnalysesBySrc :: Connection -> T.Text -> Handler [RecentAnalysis]
getAnalysesBySrc conn txt = do
  list <- liftIO $ getNDayAnalyses conn txt 10
  let result = map (\(hsh,src,mb1,mb2,mb3) -> RecentAnalysis hsh src mb1 mb2 mb3) list
  return result

getARB :: TMVar [(FilePath, (UTCTime, [[ARBText]]))] -> Handler [[[ARBText]]]
getARB arbs = do
  arbs' <- liftIO $ atomically (readTMVar arbs)
  let result = map (\(_,(_,xs)) -> xs) arbs'
  return result
