{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module SemanticParserAPI.Compute.Reuters where

import           Control.Lens                      ((^.),(^..),_1,_2,_Left,_Right,to,traverse)
import           Control.Monad                     (forM,when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL

import           Data.Function                     (on)
import           Data.List                         (groupBy,sortBy)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock                   (UTCTime)
--
import           NLP.Shared.Type                   (EventClass(..)
                                                   ,PathConfig(..)
                                                   ,Summary
                                                   ,mgstore
                                                   )
import           NLP.Semantics.Type                (ARB(..)
                                                   ,objectB,predicateR,subjectA,po_main
                                                   )
import           NLP.Type.TagPos                   (TagPos,TokIdx)
import           SRL.Analyze.Type                  (MeaningGraph)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Load                     (getFileListRecursively)


type EventCard = (FilePath,(UTCTime,([ARB],[TagPos TokIdx (EntityMention Text)],[EventClass])), Maybe Summary)



defaultTime :: Text
defaultTime = "19800101000000"


loadExistingMG :: PathConfig -> Int -> IO [Maybe MeaningGraph]
loadExistingMG cfg numMG  = do
  fps_mg <- getFileListRecursively (cfg^.mgstore)
  let fps = fps_mg
  forM (take numMG $ zip [1..] fps) $ \(i :: Int,fp) -> do
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


isWithObjOrWhiteListed :: ARB -> Bool
isWithObjOrWhiteListed y =
       check y
    && all isWithObjOrWhiteListed (y^..objectB.traverse._2._Left.po_main)
  where
    check x = (x^.objectB.to (not.null)) || (x^.predicateR._1 `elem` whiteList)


haveCommaEntity :: ARB -> Bool
haveCommaEntity y =
       check y
    || all check (y^..objectB.traverse._2._Left.po_main)
  where
    check x = (x^.subjectA._2 == ",") || any (== ",") (x^..objectB.traverse._2._Right.po_main)


isSubjectBlackListed :: ARB -> Bool
isSubjectBlackListed x = x ^.subjectA._2.to T.toLower `elem` blackList

isSubjectEmpty :: ARB -> Bool
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


sortEventCardByTime :: [(a, (UTCTime, b), c)] -> [(a, (UTCTime, b), c)]
sortEventCardByTime = sortEventCardBy (\(_,(ct,_),_) -> ct)
  where
    sortEventCardBy f ecs = sortBy (flip compare `on` f) ecs
