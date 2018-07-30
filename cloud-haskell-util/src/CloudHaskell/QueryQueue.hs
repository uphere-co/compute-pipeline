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
                     deriving Show


type QueryQueue q r = IntMap (QueryStatus q r)


type QQVar q r = TVar (QueryQueue q r)


getNewQuery :: QueryStatus q r -> Maybe q
getNewQuery (NewQuery q) = Just q
getNewQuery _            = Nothing


getAnswered :: QueryStatus q r -> Maybe r
getAnswered (Answered _ r) = Just r
getAnswered _                  = Nothing


emptyQQ :: QueryQueue q r
emptyQQ = IM.empty


newQuery :: q -> QueryQueue q r -> (Int,QueryQueue q r)
newQuery q qq = if IM.null qq
                then (0,IM.singleton 0 (NewQuery q))
                else let k = fst (IM.findMax qq) + 1
                     in (k,IM.insert k (NewQuery q) qq)


next :: QueryQueue q r -> Maybe (Int,q)
next = listToMaybe . mapMaybe (\(k,v) -> (k,) <$> getNewQuery v) . IM.toList


remove :: Int -> QueryQueue q r -> QueryQueue q r
remove i = IM.update (\_ -> Nothing) i


singleQuery :: QQVar q r -> q -> IO r
singleQuery qqvar query  = do
  i <- atomically $ do
    qq <- readTVar qqvar
    let (i,qq') = newQuery query qq
    writeTVar qqvar qq'
    pure i
  r <- atomically $ do
    qq <- readTVar qqvar
    case getAnswered =<< IM.lookup i qq of
      Nothing -> retry
      Just r -> let qq' = remove i qq
                in writeTVar qqvar qq' >> pure r
  pure r
