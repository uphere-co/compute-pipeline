{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{-# LANGUAGE TypeOperators #-}

module Query.App.API where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens                      ((^.),_1,_2)
import           Control.Monad                     (forever,forM,void)
import           Control.Monad.IO.Class            (liftIO)
-- import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Function                     (on)
import           Data.Hashable
-- import           Data.HashSet                      (HashSet)
-- import qualified Data.HashSet               as HS
import           Data.List                         (groupBy,sortBy,sortOn)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
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
import           NLP.Shared.Type                   (ARB(..),PrepOr(..),RecentAnalysis(..),RecentArticle(..))
--
import           Pipeline.Load
import           Pipeline.Type
import           Pipeline.Util                     (bstrHashToB16)


--
-- this is orphan instances but for the time being
--
instance Hashable (PrepOr Text)

instance Hashable ARB



updateARB :: FilePath -> TVar [(FilePath,(UTCTime,[ARB]))] -> IO ()
updateARB savepath arbs = do
  forever $ do
    arbs' <- readTVarIO arbs
    let cfps = map fst arbs'
    fps <- getFileListRecursively savepath
    let newarbs' = filter (\x -> not $ x `elem` cfps) fps

    newarbs'' <- forM newarbs' $ \fp -> do
      bstr <- B8.readFile fp
      return $ (,) <$> Just fp <*> A.decode (BL.fromStrict bstr)
    let newarbs = catMaybes newarbs''
    putStrLn ("number of new A-R-Bs is " ++ show (length newarbs))
    if (not $ null newarbs)
      then atomically (writeTVar arbs (arbs' ++ newarbs))
      else atomically (writeTVar arbs arbs')
    threadDelay 3000000


loadExistingARB :: FilePath -> IO [Maybe (FilePath,(UTCTime,[ARB]))]
loadExistingARB savepath = do
  fps' <- getFileListRecursively savepath
  let fps = filter (\x -> '_' `elem` x) fps'
  forM fps $ \fp -> do
    bstr <- B8.readFile fp
    return $ (,) <$> Just fp <*> A.decode (BL.fromStrict bstr)


oneDayArticles :: Connection -> Text -> IO [Ar.ArticleH]
oneDayArticles conn txt = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime
  articles <- getArticleBySourceAndTime conn (T.unpack txt) yesterday
  return articles


getOneDayArticles :: Connection -> Text -> IO [(Int,Text,Text)]
getOneDayArticles conn txt = do
  articles <- oneDayArticles conn txt
  let aList = map (\x -> (Ar._id x, T.pack $ bstrHashToB16 $ Ar._sha256 x, Ar._source x)) articles
  return aList


nDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [An.AnalysisH]
nDayAnalyses conn txt n = do
  ctime <- getCurrentTime
  let nBeforeDays = addUTCTime (-(nominalDay * n)) ctime
  analyses <- getAnalysisBySourceAndTime conn (T.unpack txt) nBeforeDays
  return analyses


getNDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [(Text,Text,Maybe Bool,Maybe Bool,Maybe Bool)]
getNDayAnalyses conn txt n = do
  analyses <- nDayAnalyses conn txt n
  let anList = map (\x -> (T.pack $ bstrHashToB16 $ An._sha256 x, An._source x, An._corenlp x, An._srl x, An._ner x)) analyses
  return anList

type API =    "recentarticle" :> Capture "ASource" T.Text :> Get '[JSON] [RecentArticle]
         :<|> "recentanalysis" :> Capture "AnSource" T.Text :> Get '[JSON] [RecentAnalysis]
         :<|> "recentarb" :> Get '[JSON] [(FilePath,(UTCTime,[ARB]))]

recentarticleAPI :: Proxy API
recentarticleAPI = Proxy

run :: Connection -> PathConfig -> IO ()
run conn cfg = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  let arbstore = (_arbstore cfg)
  arbs <- newTVarIO []
  exstarbs <- loadExistingARB arbstore
  putStrLn ("number of existing A-R-Bs is " ++ show (length exstarbs))
  atomically (writeTVar arbs (catMaybes exstarbs))
  void $ forkIO $ updateARB arbstore arbs
  runSettings settings =<< (mkApp conn arbs)


mkApp :: Connection -> TVar [(FilePath, (UTCTime, [ARB]))] -> IO Application
mkApp conn arbs = return $ simpleCors (serve recentarticleAPI (server conn arbs))


server :: Connection -> TVar [(FilePath, (UTCTime, [ARB]))] -> Server API
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


filterARB :: Int -> [(FilePath,(UTCTime,[ARB]))] -> [(FilePath,(UTCTime,[ARB]))]
filterARB n arbs =
  let -- we start with two times more sets considering filter-out items.
      arbs1 = take (2*n) $ sortBy (flip compare `on` (\(_,(ct,_)) -> ct)) arbs
      templst = do (f,(t,arbs'')) <- arbs1
                   arb <- arbs''
                   return (hash arb,f,t,arb)
      templst1 = (map (\(_h,t,f,x) -> ((t,f),x)) . map head . groupBy ((==) `on` (^._1)) . sortBy (compare `on` (^._1))) templst
      grouper lst = let ((t,f),_) = head lst
                        rs = map (^._2) lst
                    in (t,(f,rs))
      arbs2 = (map grouper . groupBy ((==) `on` (^._1)) .  sortBy (compare `on` (^._1))) templst1
  in take n $ sortBy (flip compare `on` (\(_,(ct,_)) -> ct)) arbs2



getARB :: TVar [(FilePath, (UTCTime, [ARB]))] -> Handler [(FilePath,(UTCTime,[ARB]))]
getARB arbs = do
  liftIO $ putStrLn "getARB called"
  arbs1 <- liftIO $ readTVarIO arbs
  let n = 100
  return (filterARB n arbs1)
