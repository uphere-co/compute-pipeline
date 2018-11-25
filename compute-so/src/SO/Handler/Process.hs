{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -w #-}
--
-- Module for cloud haskell process entry points.
-- This module provides main and remote table.
-- Note that registering mechanism in CH here is manual with static
-- pointer, with an intent to make the process explicit.
--
module SO.Handler.Process
  ( -- * Types and Lenses
    StateCloud(..)
  , cloudSlaves
    -- * Main process
  , main
    -- * Remote table
  , rtable
  ) where

import           Control.Concurrent (MVar, putMVar )
import           Control.Concurrent.STM   ( TVar
                                          , atomically, readTVar, retry
                                          )
import           Control.Distributed.Process.Lifted
                                          ( Process, ProcessId(..), RemoteTable, spawn )
import           Control.Distributed.Process.Node.Lifted
                                          ( initRemoteTable )
import           Control.Distributed.Static ( registerStatic, staticClosure, staticPtr )
import           Control.Error.Safe       ( headZ )
import           Control.Lens             ( (^.), at, makeLenses, to )
import           Control.Monad            ( void )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Default             ( Default(..) )
import           Data.IntMap              ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe               ( maybe )
import           Data.Rank1Dynamic        ( toDynamic )
import           GHC.Generics             ( Generic )
------
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP
                                          , queryCoreNLP
                                          )
import           Worker.Type              ( StatusProc )


-- | State that keeps the current available slaves.
data StateCloud = StateCloud { _cloudSlaves :: [ProcessId] }
                deriving (Show,Eq,Ord,Generic)

makeLenses ''StateCloud

instance Default StateCloud where
  def = StateCloud []

-- | Entry point of main CH process.
--   All the tasks are done inside main by sending process to remote workers.
--
main :: -- TVar StatusProc -> QQVar QCoreNLP RCoreNLP -> MVar (IO ()) -> Pipeline ()
     TVar StateCloud
  -> Pipeline ()
main rCloud = do -- rJava rQQ ref_jvm = do
  tellLog "start mainProcess"
  slave <-
    liftIO $ atomically $ do
      cloud <- readTVar rCloud
      maybe retry pure (cloud ^. cloudSlaves . to headZ)
  tellLog ("got a slave: " ++ show slave)
  -- liftIO $ putMVar ref_jvm (queryCoreNLP rJava rQQ)
  let slaveNode = processNodeId slave
  void $ spawn slaveNode (staticClosure (staticPtr (static testProcess)))



rtable :: RemoteTable
rtable =
  registerStatic
    "$testProcess"
    (toDynamic (staticPtr (static testProcess)))
    initRemoteTable

testProcess :: Process ()
testProcess = do
  liftIO $ putStrLn "nuclear missile launched."
