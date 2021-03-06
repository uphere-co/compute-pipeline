{-# LANGUAGE TupleSections #-}
module CloudHaskell.QueryQueue where

import           Control.Concurrent     ( forkIO, killThread, myThreadId )
import           Control.Concurrent.STM ( STM
                                        , TVar
                                        , atomically
                                        , modifyTVar'
                                        , readTVar
                                        , retry
                                        , writeTVar
                                        )
import           Control.Monad          ( forever )
import           Control.Monad.IO.Class ( MonadIO(liftIO) )
import           Data.IntMap            ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe
------
import           Worker.Type            ( StatusProc(..) )


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


waitQuery :: QQVar q r -> STM (Int,q)
waitQuery qqvar = do
  qq <- readTVar qqvar
  case next qq of
    Nothing -> retry -- wait until the next
    Just (i,q) -> do
      let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
      writeTVar qqvar qq'
      pure (i,q)


untilKilled :: TVar StatusProc -> IO () -> IO ()
untilKilled rProc action = do
  tid <- myThreadId
  forkIO $ do
    -- wait until kill signal
    atomically $ do
      s <- readTVar rProc
      case s of
        ProcKilled -> pure ()
        _ -> retry
    killThread tid
  forever action


-- | simplest, unbounded handle query
handleQuery :: (MonadIO m) => QQVar q r -> (q -> m r) -> m ()
handleQuery rQQ handler =
  forever $ do
    -- wait for a new query
    (i,q) <-
      liftIO $ atomically $ waitQuery rQQ
    -- main handler
    r <- handler q
    -- reply
    liftIO $ atomically $
      modifyTVar' rQQ (IM.update (\_ -> Just (Answered q r)) i)


handleQueryInterrupted ::
     TVar StatusProc
  -> QQVar q r
  -> (q -> IO r)
  -> IO ()
handleQueryInterrupted rProc rQQ handler =
  untilKilled rProc $ do
    (i,q) <- atomically $ waitQuery rQQ
    r <- handler q
    atomically $ modifyTVar' rQQ (IM.update (\_ -> Just (Answered q r)) i)
