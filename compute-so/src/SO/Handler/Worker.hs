{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Worker
  ( workerMain
  ) where

import           Control.Concurrent       ( MVar, ThreadId, forkFinally )
import           Control.Concurrent.STM   ( TMVar, TVar
                                          , atomically, newTVarIO
                                          , modifyTVar'
                                          , putTMVar
                                          , readTVarIO, writeTVar
                                          )
import           Control.Distributed.Process.Lifted
                                          ( ProcessId , expect, getSelfPid )
import           Control.Distributed.Process.Node
                                          ( LocalNode, closeLocalNode
                                          , newLocalNode, runProcess
                                          )
import           Control.Lens             ( (%~) )
import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Trans.Reader ( runReaderT )
import           Data.Default             ( def )
import           Data.Foldable            ( traverse_ )
import qualified Data.Text as T
------
import           CloudHaskell.Client      ( heartBeatHandshake )
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Server      ( withHeartBeat )
import           CloudHaskell.Util        ( expectSafe
                                          , handleErrorLog
                                          , newLogLock
                                          , onKill
                                          , tellLog
                                          , withTransport
                                          )
import           CloudHaskell.Type        ( Pipeline )
import           Network.Transport.UpHere ( DualHostPortPair(..) )
import           Task.CoreNLP             ( QCoreNLP, RCoreNLP )
import           Worker.Type              ( CellConfig(..)
                                          , NetworkConfig(..)
                                          , StatusProc(..)
                                          , WorkerRole(..)
                                          )

------
import           SO.Handler.Process       ( StateCloud
                                          , cloudSlaves
                                          , main
                                          , mainSlave
                                          , rtable
                                          )




master ::
  --    TVar StatusProc
     TVar StateCloud
  -> QQVar QCoreNLP RCoreNLP
  -> TMVar ProcessId
  -- -> MVar (IO ())
  -> Pipeline ()
master {- rJava -} rCloud rQQ ref {- ref_jvm -} = do
  self <- getSelfPid
  tellLog ("master self pid = " ++ show self)
  liftIO $ atomically $ putTMVar ref self

  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (\_ -> pure ()) $ \slaveId -> do
    liftIO $ atomically $
      modifyTVar' rCloud (cloudSlaves %~ (++ [slaveId]))
    main rCloud rQQ -- rJava qqvar ref_jvm
    () <- expect  -- for idling
    pure ()




slave ::
   --   TVar StatusProc
     TMVar ProcessId
  -> ProcessId
  --- > MVar (IO ())
  -> Pipeline ()
slave  _ref masterPing {- _ref_jvm -} = do
  heartBeatHandshake masterPing $ do
    mainSlave
    () <- expect
    pure ()


mkDHPP :: NetworkConfig -> DualHostPortPair
mkDHPP cfg = DHPP
                  (T.unpack (hostg cfg), show (port cfg))
                  (T.unpack (hostg cfg), show (port cfg))


killLocalNode :: TVar (Maybe LocalNode) -> IO ()
killLocalNode ref_node = do
  mnode <- readTVarIO ref_node
  traverse_ closeLocalNode mnode


-- NOTE: This should be asynchronous task, i.e. it forks a thread
--       and return the id of the thread.
workerMain ::
     QQVar QCoreNLP RCoreNLP
  -- -> TVar StatusProc
  -> TMVar ProcessId
  -> (WorkerRole,CellConfig)
  -- -> MVar (IO ())
  -> IO ThreadId
workerMain rQQ {- rJava -} ref (role,cellcfg) = do
  let dhpp = mkDHPP (cellAddress cellcfg)
  ref_node <- newTVarIO Nothing
  flip forkFinally (onKill (putStrLn "killed" >> killLocalNode ref_node)) $
    withTransport dhpp $ \transport -> do
      putStrLn "transport started"
      node <- newLocalNode transport rtable
      atomically $ writeTVar ref_node (Just node)
      lock <- newLogLock 0
      runProcess node $
        flip runReaderT lock $
          handleErrorLog $
            case role of
              Master _     -> do
                rCloud <- liftIO $ newTVarIO def
                master rCloud rQQ ref
              Slave _ mpid -> slave ref mpid
