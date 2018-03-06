{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module SemanticParserAPI.Compute.Reuters where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception                 (IOException,try)
import           Control.Lens                      ((^.),(^..),_1,_2,_Left,_Right,to,traverse)
import           Control.Monad                     (forever,forM,void,when)
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                       (rights)

import           Data.Function                     (on)
import           Data.Hashable
import           Data.List                         (groupBy,notElem,sortBy,sortOn)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
-- import           Database.PostgreSQL.Simple        (Connection)
-- import           DB.Operation                      (getRSSAnalysisBySourceAndTime,getRSSArticleBySourceAndTime)
-- import           DB.Util                           (bstrHashToB16)
-- import           Network.Wai
-- import           Network.Wai.Handler.Warp
-- import           Network.Wai.Middleware.Cors
-- import           Servant
import           System.FilePath                   (takeExtension, takeBaseName)
import           System.IO
--
-- import qualified DB.Schema.RSS.Analysis        as An
-- import qualified DB.Schema.RSS.Article         as Ar
import           NLP.Shared.Type                   (EventClass(..)
                                                   ,PathConfig(..),ItemRSS
                                                   ,link,arbstore,mgdotfigstore,mgstore
                                                   )
import           NLP.Semantics.Type                (ARB(..),PrepOr(..)
                                                   ,objectB,predicateR,subjectA,po_main
                                                   )
import           NLP.Type.TagPos                   (TagPos,TokIdx)
import           RSS.Data                          (rssAnalysisList)
import           SRL.Analyze.Type                  (MeaningGraph)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Load                     (getFileListRecursively)
-- import           Pipeline.Type


--
-- this is orphan instances but for the time being
--
-- instance Hashable (PrepOr Text)
-- instance Hashable ARB
-- instance Hashable (PrepOr ARB)

type EventCard = (FilePath,(UTCTime,([ARB],[TagPos TokIdx (EntityMention Text)],[EventClass])), Maybe ItemRSS)

{-
rssAnalysisList :: [(Source,Section,RSSLink)]
rssAnalysisList =
  [ -- ("reuters","companyNews","http://feeds.reuters.com/reuters/companyNews")
  -- , ("reuters","technologyNews","http://feeds.reuters.com/reuters/technologyNews")
  -- ,
    ("reuters","Archive","http://www.reuters.com/resources/archive/us")
  {- , ("cnbc","business","https://www.cnbc.com/id/10001147/device/rss/rss.html")
  , ("cnbc","economy","https://www.cnbc.com/id/20910258/device/rss/rss.html")
  , ("cnbc","finance","https://www.cnbc.com/id/10000664/device/rss/rss.html")
  , ("cnbc","technology","https://www.cnbc.com/id/19854910/device/rss/rss.html")
  , ("marketwatch","topstories","http://feeds.marketwatch.com/marketwatch/topstories")
  , ("marketwatch","marketpulse","http://feeds.marketwatch.com/marketwatch/marketpulse") -- Paragraph  -}
  ]
-}


defaultTime :: Text
defaultTime = "19800101000000"

maxN = 1000


{-
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
-}

loadExistingMG :: PathConfig -> IO [Maybe MeaningGraph] --  [Maybe EventCard]
loadExistingMG cfg  = do
  {- let arbpath = cfg^.arbstore
      dotpath = cfg^.mgdotfigstore
  fps_arb <- getFileListRecursively arbpath -}
  -- print arbpath
  --print fps_arb
  {- fps_dot <- map takeBaseName . filter (\x -> takeExtension x == ".png") <$> getFileListRecursively dotpath -}
  -- let fps = filter (\x -> ('_' `elem` x) && (takeBaseName x `elem` fps_dot)) fps_arb
  fps_mg <- getFileListRecursively (cfg^.mgstore)
  let fps = fps_mg
  forM (take 100 $ zip [1..] fps) $ \(i,fp) -> do
    when (i `mod` 1000 == 0) $ do
      putStrLn (show i ++ "th file")

    bstr <- B8.readFile fp
    {- let hsh = fst $ T.breakOn "_" $ T.pack $ takeBaseName fp
    let sfps = map (\(x,y,_) -> T.intercalate "/" [T.pack (_rssstore cfg),T.pack x,T.pack y,"RSSItem",(T.take 2 hsh),hsh]) rssAnalysisList
    (ebstrs :: [Either IOException B8.ByteString]) <- liftIO $ mapM (\sfp -> try $ B8.readFile (T.unpack sfp)) sfps
    let sbstrs = rights ebstrs
    mitem <- case sbstrs of
               []   -> return Nothing
               x:xs -> let (mitem :: Maybe ItemRSS) = (A.decode . BL.fromStrict) x in return mitem -}
    return (A.decode' (BL.fromStrict bstr))





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

isSubjectEmpty x = T.null (x^.subjectA._2)


filterARB :: Int
          -> [EventCard]
          -> [EventCard]
filterARB n arbs =
  let arbs0 = map (\(f,(t,(xs,ner,evt)),mitem)-> (f,(t,(filter (\x -> not (isSubjectEmpty x) &&  {- not (isSubjectBlackListed x) && isWithObjOrWhiteListed x  && -} (not (haveCommaEntity x) ) ) xs,ner,evt)),mitem)) arbs
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


{-
getARB :: TVar [EventCard]
       -> Int
       -> Handler [EventCard]
getARB arbs i = do
  liftIO $ putStrLn "getARB called"
  arbs1 <- liftIO $ readTVarIO arbs
  liftIO $ print (length arbs1)
  let n = 1000
      result = take n (drop (n*i) arbs1)
  liftIO $ mapM_ print (take 3 result)
  return result
-}

sortEventCardBy f ecs = sortBy (flip compare `on` f) ecs

sortEventCardByTime = sortEventCardBy (\(_,(ct,_),_) -> ct)
