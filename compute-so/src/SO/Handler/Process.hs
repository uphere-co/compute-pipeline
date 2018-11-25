{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module SO.Handler.Process
  ( mainProcess
  ) where

import           Control.Concurrent (MVar, putMVar )
import           Control.Concurrent.STM   ( TMVar )
import           Control.Monad.IO.Class   ( liftIO )
------
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP
                                          , queryCoreNLP
                                          )


mainProcess :: TMVar () -> QQVar QCoreNLP RCoreNLP -> MVar (IO ()) -> Pipeline ()
mainProcess isDone qqvar ref_jvm = do
  tellLog "start mainProcess"
  liftIO $ putMVar ref_jvm (queryCoreNLP isDone qqvar)
