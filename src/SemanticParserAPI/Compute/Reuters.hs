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
import           System.FilePath                   (takeExtension, takeBaseName)
import           System.IO
--
import           NLP.Shared.Type                   (EventClass(..)
                                                   ,PathConfig(..),ItemRSS
                                                   ,link,arbstore,mgdotfigstore,mgstore
                                                   )
import           NLP.Semantics.Type                (ARB(..),PrepOr(..)
                                                   ,objectB,predicateR,subjectA,po_main
                                                   )
import           NLP.Type.TagPos                   (TagPos,TokIdx)
-- import           RSS.Data                          (rssAnalysisList)
import           SRL.Analyze.Type                  (MeaningGraph)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Load                     (getFileListRecursively)


type EventCard = (FilePath,(UTCTime,([ARB],[TagPos TokIdx (EntityMention Text)],[EventClass])), Maybe ItemRSS)



defaultTime :: Text
defaultTime = "19800101000000"


loadExistingMG :: PathConfig -> Int -> IO [Maybe MeaningGraph]
loadExistingMG cfg numMG  = do
  fps_mg <- getFileListRecursively (cfg^.mgstore)
  let fps = fps_mg
  forM (take numMG $ zip [1..] fps) $ \(i,fp) -> do
    when (i `mod` 1000 == 0) $ do
      putStrLn (show i ++ "th file")

    bstr <- B8.readFile fp
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



sortEventCardBy f ecs = sortBy (flip compare `on` f) ecs

sortEventCardByTime = sortEventCardBy (\(_,(ct,_),_) -> ct)
