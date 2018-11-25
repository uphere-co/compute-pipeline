{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Process
  ( mainProcess
  ) where

import Control.Concurrent (ThreadId, forkOS, threadDelay)
import           Control.Concurrent.STM   ( atomically, modifyTVar' )
import Control.Exception (SomeException,mask,try)
import           Control.Monad            ( forever, void )
import           Control.Monad.IO.Class   ( liftIO )
import qualified Data.IntMap as IM
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
------
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP, RCoreNLP, daemonCoreNLP )


forkOSFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkOSFinally action and_then =
  mask $ \restore ->
    forkOS $ try (restore action) >>= and_then


test = threadDelay 100000000

mainProcess :: QQVar QCoreNLP RCoreNLP -> Pipeline ()
mainProcess qqvar = do
  tellLog "mainProcess: deamonCoreNLP"
  void $ liftIO $ forkOSFinally {- test  -} (daemonCoreNLP qqvar) $ \e -> putStrLn ("mainProcess: " ++ show e)
