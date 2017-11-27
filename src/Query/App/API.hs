{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Query.App.API where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception                 (IOException,try)
import           Control.Lens                      ((^.),(^..),_1,_2,_Left,_Right,to,traverse)
import           Control.Monad                     (forever,forM,void)
import           Control.Monad.IO.Class            (liftIO)
-- import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                       (rights)

import           Data.Function                     (on)
import           Data.Hashable
-- import           Data.HashSet                      (HashSet)
-- import qualified Data.HashSet               as HS
import           Data.List                         (groupBy,notElem,sortBy,sortOn)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
import           Database.PostgreSQL.Simple        (Connection)
import           DB.Operation                      (getRSSAnalysisBySourceAndTime,getRSSArticleBySourceAndTime)
import           DB.Util                           (bstrHashToB16)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.FilePath                   (takeExtension, takeBaseName)
import           System.IO
--
import qualified DB.Schema.RSS.Analysis        as An
import qualified DB.Schema.RSS.Article         as Ar
import           NLP.Shared.Type                   (ARB(..),PrepOr(..),RecentAnalysis(..),RecentArticle(..)
                                                   ,PathConfig(..),EventClass(..),ItemRSS,link
                                                   ,arbstore,mgdotfigstore
                                                   ,objectB,predicateR,subjectA,po_main)
import           NLP.Type.TagPos                   (TagPos,TokIdx)
import           RSS.Data                          (rssAnalysisList)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Type


--
-- this is orphan instances but for the time being
--
instance Hashable (PrepOr Text)

instance Hashable ARB

instance Hashable (PrepOr ARB)

type EventCard = (FilePath,(UTCTime,([ARB],[TagPos TokIdx (EntityMention Text)],[EventClass])), Maybe ItemRSS)


updateARB :: PathConfig
          -> TVar [EventCard]
          -> TVar [EventCard]
          -> IO ()
updateARB cfg arbs arbsfiltered = do
  let arbpath = cfg^.arbstore
      dotpath = cfg^.mgdotfigstore
  forever $ do
    arbs' <- readTVarIO arbs
    arbsfiltered' <- readTVarIO arbsfiltered
    let cfps = map (^._1) (arbs' ++ arbsfiltered')
    fps_arb <- getFileListRecursively arbpath
    fps_dot <- map takeBaseName . filter (\x -> takeExtension x == ".png") <$> getFileListRecursively dotpath
    let newarbs' = filter (\x -> ('_' `elem` x) && (takeBaseName x `elem` fps_dot) && (not (x `elem` cfps))) fps_arb

    newarbs'' <- forM newarbs' $ \fp -> do
      bstr <- B8.readFile fp
      let hsh = fst $ T.breakOn "_" $ T.pack $ takeBaseName fp
      let sfps = map (\(x,y,_) -> T.intercalate "/" [T.pack (_rssstore cfg),T.pack x,T.pack y,"RSSItem",(T.take 2 hsh),hsh]) rssAnalysisList
      (ebstrs :: [Either IOException B8.ByteString]) <- liftIO $ mapM (\sfp -> try $ B8.readFile (T.unpack sfp)) sfps
      let sbstrs = rights ebstrs
      mitem <- case sbstrs of
                 []   -> return Nothing
                 x:xs -> let (mitem :: Maybe ItemRSS) = (A.decode . BL.fromStrict) x in return mitem
      return $ (,,) <$> Just fp <*> A.decode' (BL.fromStrict bstr) <*> (Just mitem)
    let newarbs = catMaybes newarbs''
        newarbs_filter_pass = filterARB maxN newarbs
        newarbs_filter_fail = filter (\a -> a `notElem` newarbs_filter_pass) newarbs
    putStrLn ("number of new A-R-Bs is " ++ show (length newarbs))
    if (not $ null newarbs)
      then do
      atomically (writeTVar arbs (sortEventCardByTime (newarbs_filter_pass ++ arbs')))
      atomically (writeTVar arbsfiltered (sortEventCardByTime (newarbs_filter_fail ++ arbsfiltered')))
      else do
      atomically (writeTVar arbs arbs')
      atomically (writeTVar arbsfiltered arbsfiltered')
    let sec = 1000000 in threadDelay (10*sec)


loadExistingARB :: PathConfig -> IO [Maybe EventCard]
loadExistingARB cfg  = do
  let arbpath = cfg^.arbstore
      dotpath = cfg^.mgdotfigstore
  fps_arb <- getFileListRecursively arbpath
  fps_dot <- map takeBaseName . filter (\x -> takeExtension x == ".png") <$> getFileListRecursively dotpath
  let fps = filter (\x -> ('_' `elem` x) && (takeBaseName x `elem` fps_dot)) fps_arb
  forM fps $ \fp -> do
    bstr <- B8.readFile fp
    let hsh = fst $ T.breakOn "_" $ T.pack $ takeBaseName fp
    let sfps = map (\(x,y,_) -> T.intercalate "/" [T.pack (_rssstore cfg),T.pack x,T.pack y,"RSSItem",(T.take 2 hsh),hsh]) rssAnalysisList
    (ebstrs :: [Either IOException B8.ByteString]) <- liftIO $ mapM (\sfp -> try $ B8.readFile (T.unpack sfp)) sfps
    let sbstrs = rights ebstrs
    mitem <- case sbstrs of
               []   -> return Nothing
               x:xs -> let (mitem :: Maybe ItemRSS) = (A.decode . BL.fromStrict) x in return mitem
    return $ (,,) <$> Just fp <*> A.decode' (BL.fromStrict bstr) <*> (Just mitem)


oneDayArticles :: Connection -> Text -> IO [Ar.RSSArticleH]
oneDayArticles conn txt = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime
  articles <- getRSSArticleBySourceAndTime conn (T.unpack txt) yesterday
  return articles


getOneDayArticles :: Connection -> Text -> IO [(Int,Text,Text)]
getOneDayArticles conn txt = do
  articles <- oneDayArticles conn txt
  let aList = map (\x -> (Ar._id x, T.pack $ bstrHashToB16 $ Ar._hash x, Ar._source x)) articles
  return aList


nDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [An.RSSAnalysisH]
nDayAnalyses conn txt n = do
  ctime <- getCurrentTime
  let nBeforeDays = addUTCTime (-(nominalDay * n)) ctime
  analyses <- getRSSAnalysisBySourceAndTime conn (T.unpack txt) nBeforeDays
  return analyses


getNDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [(Text,Text,Maybe Bool,Maybe Bool,Maybe Bool)]
getNDayAnalyses conn txt n = do
  analyses <- nDayAnalyses conn txt n
  let anList = map (\x -> (T.pack $ bstrHashToB16 $ An._hash x, An._source x, An._corenlp x, An._srl x, An._ner x)) analyses
  return anList

type API =    "recentarticle" :> Capture "ArSrc" T.Text :> Capture "ArSec" T.Text :> Capture "ArHash" T.Text :> Get '[JSON] (Maybe ItemRSS)
         :<|> "rssarticle" :> Capture "ArHash" T.Text :> Get '[JSON] (Maybe ItemRSS)
         :<|> "recentanalysis" :> Capture "AnSource" T.Text :> Get '[JSON] [RecentAnalysis]
         :<|> "recentarb" :> Capture "n" Int :> Get '[JSON] [EventCard]

recentarticleAPI :: Proxy API
recentarticleAPI = Proxy

maxN = 1000000

run :: Connection -> PathConfig -> Int -> IO ()
run conn cfg port = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  let arbstore = (_arbstore cfg)
  arbs <- newTVarIO []
  arbsfiltered <- newTVarIO []
  mexstarbs <- loadExistingARB cfg -- arbstore
  let exstarbs = catMaybes mexstarbs
      exstarbs_filter_pass = filterARB maxN exstarbs
      exstarbs_filter_fail = filter (\a -> a `notElem` exstarbs_filter_pass) exstarbs
  putStrLn ("number of existing A-R-Bs is " ++ show (length exstarbs))
  atomically (writeTVar arbs exstarbs_filter_pass)
  atomically (writeTVar arbsfiltered exstarbs_filter_fail)
  putStrLn "Loading is completed."
  void $ forkIO $ updateARB cfg {- arbstore  -} arbs arbsfiltered
  runSettings settings =<< (mkApp conn cfg arbs)


mkApp :: Connection
      -> PathConfig
      -> TVar [EventCard]
      -> IO Application
mkApp conn cfg arbs = return $ simpleCors (serve recentarticleAPI (server conn cfg arbs))


server :: Connection
       -> PathConfig
       -> TVar [EventCard]
       -> Server API
server conn cfg arbs = (getArticlesBySrc conn cfg) :<|> (getRSSArticle conn cfg) :<|> (getAnalysesBySrc conn) :<|> (getARB arbs)


getArticlesBySrc :: Connection -> PathConfig -> T.Text -> T.Text -> T.Text -> Handler (Maybe ItemRSS)
getArticlesBySrc conn cfg src sec hsh = do
  let filepath = (T.intercalate "/" [T.pack (_rssstore cfg),src,sec,"RSSItem",(T.take 2 hsh),hsh])
  ebstr <- liftIO $ try $ B8.readFile (T.unpack filepath)
  case ebstr of
    Left (_e :: IOException) -> return Nothing
    Right bstr -> do
      let mitem = (A.decode . BL.fromStrict) bstr
      return mitem

getRSSArticle :: Connection -> PathConfig -> T.Text -> Handler (Maybe ItemRSS)
getRSSArticle conn cfg hsh = do
  let fps = map (\(x,y,_) -> T.intercalate "/" [T.pack (_rssstore cfg),T.pack x,T.pack y,"RSSItem",(T.take 2 hsh),hsh]) rssAnalysisList
  (ebstrs :: [Either IOException B8.ByteString]) <- liftIO $ mapM (\fp -> try $ B8.readFile (T.unpack fp)) fps
  let bstrs = rights ebstrs
  case bstrs of
    []   -> return Nothing
    x:xs -> let (mitem :: Maybe ItemRSS) = (A.decode . BL.fromStrict) x in case mitem of
      Nothing   -> return Nothing
      Just item -> return (Just item) --  (item ^. link)

getAnalysesBySrc :: Connection -> T.Text -> Handler [RecentAnalysis]
getAnalysesBySrc conn txt = do
  list <- liftIO $ getNDayAnalyses conn txt 10
  let result = map (\(hsh,src,mb1,mb2,mb3) -> RecentAnalysis hsh src mb1 mb2 mb3) list
  return result


whiteList :: [Text]
whiteList = [ "Ceasing_to_be", "Success_or_failure" , "Process_start", "Process_stop", "Process_pause", "Process_continue", "Process_end"
            , "Self_motion", "Arriving"
            , "Expansion"
            ]


blackList :: [Text]
blackList = [ "he", "we", "i", "she", "they", "you", "it"
            , "this", "that", "these", "those"
            , "and", "but", "to", "*"
            , "-lrb-", "-rrb-", "-lsb-", "-rsb-"
            ]


isWithObjOrWhiteListed x = check x && all isWithObjOrWhiteListed (x^..objectB.traverse._2._Left.po_main)
  where check x = (x^.objectB.to (not.null)) || (x^.predicateR._1 `elem` whiteList)


haveCommaEntity x = check x || all check (x^..objectB.traverse._2._Left.po_main)
  where check x = (x^.subjectA._2 == ",") || any (== ",") (x^..objectB.traverse._2._Right.po_main)


isSubjectBlackListed x = x ^.subjectA._2.to T.toLower `elem` blackList


filterARB :: Int
          -> [EventCard]
          -> [EventCard]
filterARB n arbs =
  let arbs0 = map (\(f,(t,(xs,ner,evt)),mitem)-> (f,(t,(filter (\x -> not (isSubjectBlackListed x) && isWithObjOrWhiteListed x && (not (haveCommaEntity x))) xs,ner,evt)),mitem)) arbs
      -- we start with two times more sets considering filter-out items.
      arbs1 = take (2*n) $ sortBy (flip compare `on` (\(_,(ct,_),_) -> ct)) arbs0
      templst = do (f,(t,(arbs'',ner,evt)),mitem) <- arbs1
                   arb <- arbs''
                   return (f,t,(arb,ner,evt),mitem)
      templst1 = (map (\(f,t,x,mitem) -> ((f,t),x,mitem)) . map head . groupBy ((==) `on` (^._1)) . sortBy (compare `on` (^._1))) templst
      grouper lst = let ((f,t),(_,ner,evt),mitem) = head lst
                        rs = map (^._2._1) lst
                    in (f,(t,(rs,ner,evt)),mitem)
      arbs2 = (map grouper . groupBy ((==) `on` (^._1)) .  sortBy (compare `on` (^._1))) templst1
  in take n $ sortEventCardByTime arbs2


getARB :: TVar [EventCard]
       -> Int
       -> Handler [EventCard]
getARB arbs i = do
  liftIO $ putStrLn "getARB called"
  arbs1 <- liftIO $ readTVarIO arbs
  let n = 500
      result = take n (drop (n*i) arbs1)
  liftIO $ mapM_ print (take 3 result)
  return result

sortEventCardBy f ecs = sortBy (flip compare `on` f) ecs

sortEventCardByTime = sortEventCardBy (\(_,(ct,_),_) -> ct)
