{-# LANGUAGE TupleSections #-}

module CloudHaskell.QueryQueue where

import           Control.Concurrent.STM
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap            as IM
import           Data.Maybe
--
import           Prelude


data QueryStatus q r = NewQuery q
                     | BeingProcessed q
                     | Answered q r
                     | Removed
                     deriving Show


getNewQuery :: QueryStatus q r -> Maybe q
getNewQuery (NewQuery q) = Just q
getNewQuery _            = Nothing


getAnswered :: QueryStatus q r -> Maybe r
getAnswered (Answered _ r) = Just r
getAnswered _                  = Nothing


isRemoved :: QueryStatus q r -> Bool
isRemoved Removed = True
isRemoved _       = False


type QueryQueue q r = IntMap (QueryStatus q r)


emptyQQ :: QueryQueue q r
emptyQQ = IM.empty


type QQVar q r = TVar (QueryQueue q r)


newQuery :: q -> QueryQueue q r -> (Int,QueryQueue q r)
newQuery q qq = if IM.null qq
                then (0,IM.singleton 0 (NewQuery q))
                else let k = fst (IM.findMax qq) + 1
                     in (k,IM.insert k (NewQuery q) qq)


clean :: QueryQueue q r -> QueryQueue q r
clean = IM.filter (not.isRemoved) 


next :: QueryQueue q r -> Maybe (Int,q)
next = listToMaybe . mapMaybe (\(k,v) -> (k,) <$> getNewQuery v) . IM.toList


remove :: Int -> QueryQueue q r -> QueryQueue q r
remove i = IM.update (\_ -> Just Removed) i


singleQuery :: QQVar q r -> q -> IO r
singleQuery qqvar query  = do
  i <- atomically $ do
    qq <- readTVar qqvar
    let (i,qq') = newQuery query qq
    writeTVar qqvar qq'
    return i
  r <- atomically $ do
    qq <- readTVar qqvar
    case getAnswered =<< IM.lookup i qq of
      Nothing -> retry
      Just r -> let qq' = remove i qq
                in writeTVar qqvar qq' >> return r
  return r
