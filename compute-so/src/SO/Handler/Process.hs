{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module SO.Handler.Process
  ( main
  ) where

import           Control.Concurrent (MVar, putMVar )
import           Control.Concurrent.STM   ( TVar )
import           Control.Monad.IO.Class   ( liftIO )
------
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP
                                          , queryCoreNLP
                                          )
import           Worker.Type              ( StatusProc )

main :: TVar StatusProc -> QQVar QCoreNLP RCoreNLP -> MVar (IO ()) -> Pipeline ()
main rJava rQQ ref_jvm = do
  tellLog "start mainProcess"
  liftIO $ putMVar ref_jvm (queryCoreNLP rJava rQQ)
