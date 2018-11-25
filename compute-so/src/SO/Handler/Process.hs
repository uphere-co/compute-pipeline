{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Process
  ( jvmApp
  , mainProcess
  ) where

import Control.Concurrent (MVar, ThreadId, forkOS, putMVar, threadDelay)
import           Control.Concurrent.STM   ( TMVar, atomically, modifyTVar' )
import Control.Exception (SomeException,mask,try)
import           Control.Monad            ( forever, void )
import           Control.Monad.IO.Class   ( liftIO )
import qualified Data.IntMap as IM
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
import qualified Foreign.JNI.Types as JNI
------
import           CloudHaskell.QueryQueue  ( QQVar, handleQuery )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP
                                          -- , daemonCoreNLP
                                          -- , prepareAndProcess
                                          , queryCoreNLP
                                          )


forkOSFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkOSFinally action and_then =
  mask $ \restore ->
    forkOS $ try (restore action) >>= and_then


test = threadDelay 100000000

mainProcess :: TMVar () -> QQVar QCoreNLP RCoreNLP -> MVar (IO ()) -> Pipeline ()
mainProcess isDone qqvar ref_jvm = do
  tellLog "start mainProcess"
  liftIO $ putMVar ref_jvm (jvmApp isDone qqvar)
  -- void $ liftIO $ forkOSFinally {- test  -} (daemonCoreNLP qqvar) $ \e -> putStrLn ("mainProcess: " ++ show e)

jvmApp :: TMVar () -> QQVar QCoreNLP RCoreNLP -> IO ()
jvmApp isDone qqvar = do
  -- daemonCoreNLP qqvar jvm
  putStrLn "jvmApp started."
  queryCoreNLP isDone qqvar
{-  prepareAndProcess $ \pp ->
    handleQuery qqvar (\case QCoreNLP txt -> RCoreNLP <$> runParser pp txt)
-}
