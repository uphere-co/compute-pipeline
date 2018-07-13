{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.CLI.Client where

import           Control.Concurrent.STM              (atomically,retry
                                                     ,modifyTVar',readTVar,writeTVar)
import           Control.Distributed.Process.Lifted  (Process,SendPort,ReceivePort,spawnLocal)
import           Control.Monad                       (forever,join,when)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Loops                 (whileJust_)
import           Control.Monad.Trans.Class           (lift)
import qualified Data.IntMap                   as IM
import           Data.List                           (intercalate)
import qualified Data.Text                     as T
import           System.Console.Haskeline            (runInputT,getInputLine,defaultSettings)
import           System.Console.Haskeline.MonadException (MonadException(controlIO),RunIO(..))
--
import           CloudHaskell.QueryQueue             (QueryStatus(..),QQVar,next)
import           CloudHaskell.Type                   (LogProcess)
import           CloudHaskell.Util                   (queryProcess,tellLog)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..))

instance MonadException Process where
  controlIO f = join . liftIO $ f (RunIO return)


consoleClient :: (SendPort ComputeQuery, ReceivePort ComputeResult) -> LogProcess ()
consoleClient (sq,rr) = do
  runInputT defaultSettings $
    whileJust_ (getInputLine "% ") $ \input' ->
      when (not (null input')) $ do
        let w:ws = words input'
        case w of
          ":v" -> let input = T.pack (intercalate " " ws)
                  in lift $ queryProcess (sq,rr) (CQ_Sentence input) (liftIO . print)
          ":r" -> lift $ queryProcess (sq,rr) (CQ_Reuters 100) (liftIO . print)
          _ -> return ()



webClient :: QQVar ComputeQuery ComputeResult
          -> (SendPort ComputeQuery, ReceivePort ComputeResult)
          -> LogProcess ()
webClient qqvar (sq,rr) = do
  forever $ do
    (i,q) <- liftIO $ atomically $ do
               qq <- readTVar qqvar
               case next qq of
                 Nothing -> retry
                 Just (i,q) -> do
                   let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                   writeTVar qqvar qq'
                   return (i,q)
    tellLog ("query start: " ++ show (i,q))
    spawnLocal $ do
      r <- queryProcess (sq,rr) q return
      liftIO $ atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
      test <- liftIO $ atomically $ readTVar qqvar
      tellLog (show test)
