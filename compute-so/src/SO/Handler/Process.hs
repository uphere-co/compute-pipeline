{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Process
  ( mainProcess
  ) where

import Control.Concurrent (forkOS, threadDelay)
import           Control.Concurrent.STM   ( atomically, modifyTVar' )
import           Control.Monad            ( forever, void )
import           Control.Monad.IO.Class   ( liftIO )
import qualified Data.IntMap as IM
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
------
import           CloudHaskell.QueryQueue  ( QQVar
                                          , QueryStatus(..)
                                          , waitQuery
                                          )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP, RCoreNLP, daemonCoreNLP )


mainProcess :: QQVar QCoreNLP RCoreNLP -> Pipeline ()
mainProcess qqvar = do
  tellLog "mainProcess: deamonCoreNLP"
  void $ liftIO $ forkOS $ daemonCoreNLP qqvar
