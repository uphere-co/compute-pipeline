{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
--
-- Module for cloud haskell process entry points.
-- This module provides main and remote table.
--
module SO.Handler.Process
  ( -- * Types and Lenses
    StateCloud(..)
  , cloudSlaves
    -- * Main process
  , main      -- master
  , mainSlave -- slave
    -- * Remote table
  , rtable
  ) where

import           Control.Concurrent       ( MVar, putMVar )
import           Control.Concurrent.STM   ( TVar
                                          , atomically
                                          , newTVarIO
                                          , readTVar
                                          , retry
                                          )
import           Control.Distributed.Process.Lifted
                                          ( Process
                                          , ProcessId(..)
                                          , ReceivePort
                                          , RemoteTable
                                          , SendPort
                                          , expect
                                          , newChan
                                          , sendChan
                                          , receiveChan
                                          , spawnChannel
                                          , spawnLocal
                                          , unStatic
                                          )
import           Control.Distributed.Process.Node.Lifted
                                          ( initRemoteTable )
import           Control.Distributed.Process.Serializable
                                          ( Serializable
                                          , SerializableDict(..)
                                          )
import           Control.Distributed.Static
                                          ( Static
                                          , registerStatic
                                          , staticClosure
                                          , staticPtr
                                          )
import           Control.Error.Safe       ( headZ )
import           Control.Lens             ( (^.), makeLenses, to )
import           Control.Monad            ( forever )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Default             ( Default(..) )
import           Data.Maybe               ( maybe )
import           Data.Rank1Dynamic        ( toDynamic )
import           GHC.Generics             ( Generic )
------
import           CloudHaskell.Closure     ( capply' )
import           CloudHaskell.QueryQueue  ( QQVar
                                          , emptyQQ
                                          , handleQuery
                                          , singleQuery
                                          )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP(..)
                                          , queryCoreNLP
                                          )
import           Task.SemanticParser      ( ComputeQuery(..), ComputeResult(..)
                                          , runSRLQueryDaemon
                                          )
import           Worker.Type              ( StatusProc, javaProc, javaProcStatus )



-- | State that keeps the current available slaves.
data StateCloud = StateCloud { _cloudSlaves :: [ProcessId] }
                deriving (Show,Eq,Ord,Generic)

makeLenses ''StateCloud

instance Default StateCloud where
  def = StateCloud []


-- | Entry point of main CH process.
--   All the tasks are done inside main by sending process to remote workers.
--
main ::
     TVar StateCloud
  -> QQVar ComputeQuery ComputeResult
  -> Pipeline ()
main rCloud rQQ = do
  tellLog "start mainProcess"
  slave <-
    liftIO $ atomically $ do
      cloud <- readTVar rCloud
      maybe retry pure (cloud ^. cloudSlaves . to headZ)
  tellLog ("got a slave: " ++ show slave)
  let slaveNode = processNodeId slave
  let process =
        capply'
          (staticPtr (static (SerializableDict @(Static (TVar StatusProc)))))
          (capply'
            (staticPtr (static (SerializableDict @(Static (MVar (IO ()))))))
            (staticClosure (staticPtr (static daemonSemanticParser)))
            (staticPtr (static javaProc))
          )
          (staticPtr (static javaProcStatus))
  sQ <-
    spawnChannel
      (staticPtr (static (SerializableDict @(ComputeQuery,SendPort ComputeResult))))
      slaveNode
      process

  handleQuery rQQ $ \q -> do
    (sR,rR) <- newChan
    tellLog $ show q
    sendChan sQ (q,sR)
    r <- receiveChan rR
    pure r


-- | Global remote table.
--
--   NOTE: Registering mechanism in CH here is manual with static
--         pointer, with an intent to make the process explicit.
rtable :: RemoteTable
rtable =
    registerStatic
    "$daemonSemanticParser"
    (toDynamic (staticPtr (static daemonSemanticParser)))
  $ registerStatic
    "$daemonCoreNLP"
    (toDynamic (staticPtr (static daemonCoreNLP)))
  $ registerStatic
    "$javaProc"
    (toDynamic (staticPtr (static javaProc)))
  $ registerStatic
    "$javaProcStatus"
    (toDynamic (staticPtr (static javaProcStatus)))
  $ initRemoteTable


-- | Make a remote daemon process from local IO daemon
daemon ::
     (Serializable q, Serializable r)
  => (TVar StatusProc -> QQVar q r -> IO ())
  -> Static (MVar (IO ()))
  -> Static (TVar StatusProc)
  -> ReceivePort (q, SendPort r)
  -> Process ()
daemon queryHandler pJProc pJProcStatus rQ = do
  rQQ <- liftIO $ newTVarIO emptyQQ
  -- query request handler thread, this signals query to worker thread.
  spawnLocal $ do
    forever $ do
      -- receive query
      (q,sR) <- receiveChan rQ
      -- process query
      r <- liftIO $ singleQuery rQQ q
      -- send answer
      sendChan sR r
  -- insert process into main worker thread
  jproc <- unStatic pJProc
  jprocStatus <- unStatic pJProcStatus
  liftIO $ putMVar jproc (queryHandler jprocStatus rQQ)
  -- idling.
  () <- expect
  pure ()


-- | CoreNLP daemon
daemonCoreNLP ::
     Static (MVar (IO ()))
  -> Static (TVar StatusProc)
  -> ReceivePort (QCoreNLP, SendPort RCoreNLP)
  -> Process ()
daemonCoreNLP = daemon queryCoreNLP


-- | Semantic Parser daemon
daemonSemanticParser ::
     Static (MVar (IO ()))
  -> Static (TVar StatusProc)
  -> ReceivePort (ComputeQuery, SendPort ComputeResult)
  -> Process ()
daemonSemanticParser =
  daemon (runSRLQueryDaemon (True,True) "/home/wavewave/repo/srcp/uphere-ops/api-dev-2/lang-config.json.mark")



mainSlave :: Pipeline ()
mainSlave = do
  tellLog "mainSlave"
