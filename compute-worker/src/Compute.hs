{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}
module Compute
  ( masterMain
  , slaveMain
  ) where

import           Control.Concurrent.STM                    ( TVar
                                                           , atomically
                                                           , newTVarIO
                                                           , readTVar
                                                           ,  writeTVar
                                                           )
import           Control.Distributed.Process.Lifted        ( ProcessId, SendPort
                                                           , processNodeId
                                                           , expect, getSelfPid, send
                                                           , newChan, receiveChan
                                                           , spawnLocal, spawnChannel
                                                           )
import           Control.Distributed.Process.Node          ( newLocalNode,runProcess)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Distributed.Static                (staticClosure
                                                           ,staticPtr)
import           Control.Exception                         (bracket)
import           Control.Lens                              ((&),(.~),(^.),(%~)
                                                           ,at,_1,_2)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T  (unpack)
import           Network.Transport                         (closeTransport)
---------------- compute-pipeline
import qualified CloudHaskell.Client                       ( slaveMain )
import           CloudHaskell.Closure                      ( capply' )
import           CloudHaskell.Server                       (server,withHeartBeat)
import           CloudHaskell.Type                         ( Gateway(..)
                                                           , Pipeline
                                                           , TCPPort(..)
                                                           , Router(..)
                                                           , MasterConfig(..)
                                                           , SlaveConfig(..)
                                                           )
import           CloudHaskell.Util                         ( RequestDuplex
                                                           , tellLog
                                                           , expectSafe
                                                           , tryCreateTransport
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           Task.CoreNLP                              (QCoreNLP(..),RCoreNLP(..))
---------------- this package
import           Compute.Task                              ( remoteDaemonCoreNLP
                                                           , rtable
                                                           )
import           Compute.Type.Status                       ( NodeStatus(..)
                                                           , nodeStatusMainProcessId
                                                           , nodeStatusIsServing
                                                           , nodeStatusNumServed
                                                           , Status
                                                           , statusNodes
                                                           )


elimLinkedProcess :: TVar Status -> ProcessId -> Pipeline ()
elimLinkedProcess ref pid = do
  liftIO $ atomically $ do
    m <- readTVar ref
    let elim i = HM.map $ \case Nothing -> Nothing
                                Just nstat -> if (nstat^.nodeStatusMainProcessId == i)
                                              then Nothing
                                              else Just nstat
        m' = m & (statusNodes %~ elim pid)
    writeTVar ref m'


addLinkedProcess ::
     TVar Status
  -> (Text,ProcessId,RequestDuplex QCoreNLP RCoreNLP)
  -> Pipeline ()
addLinkedProcess ref (cname,pid,duplex) =
  liftIO $ atomically $ do
    m <- readTVar ref
    let nstat = NodeStatus pid False 0 duplex
        m' = m & (statusNodes . at cname .~ Just (Just nstat))
    writeTVar ref m'

updateLinkedProcessStatus :: TVar Status -> (Text,(Bool,Int)) -> Pipeline ()
updateLinkedProcessStatus ref (cname,status) =
  liftIO $ atomically $ do
    m <- readTVar ref
    let mnstat = m ^. statusNodes . at cname
    case mnstat of
      Just (Just nstat) -> do
        let nstat' = nstat & nodeStatusIsServing .~ (status^._1)
                           & nodeStatusNumServed .~ (status^._2)
            m' = m & (statusNodes . at cname .~ Just (Just nstat'))
        writeTVar ref m'
      _ -> pure ()

launchCoreNLP :: TVar Status -> Text -> ProcessId -> Pipeline ()
launchCoreNLP ref cname pid = do
  let nid = processNodeId pid
  tellLog $ "node id = " ++ show nid
  (sr,rr) <- newChan
  (sstat,rstat) <- newChan
  -- call remoteDaemonCoreNLP in the slave node.
  -- TODO: This will be simplified using Quasi-Quoter
  sq <- spawnChannel
          (staticPtr (static (SerializableDict @QCoreNLP)))
          nid
          (capply'
            (staticPtr (static (SerializableDict @(SendPort RCoreNLP) )))
            (capply'
              (staticPtr (static (SerializableDict @(SendPort (Bool,Int)))))
              (staticClosure (staticPtr (static remoteDaemonCoreNLP)))
              sstat
            )
            sr
          )
  -- for monitoring
  spawnLocal $ forever $ do
    (b,n) <- receiveChan rstat
    updateLinkedProcessStatus ref (cname,(b,n))
  let duplex = (sq,rr)
  addLinkedProcess ref (cname,pid,duplex)


taskManager :: TVar Status -> Pipeline ()
taskManager ref = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (elimLinkedProcess ref) $ \them_main -> do
    themaster <- getSelfPid
    let router = Router $ HM.fromList [ ("master", themaster) ]
    send them_main router
    cname :: Text <- expectSafe
    tellLog $ "taskManager: got " ++ show cname
    launchCoreNLP ref cname them_main
    () <- expect  -- for idling
    pure ()


masterMain
  :: Status
  -> MasterConfig
  -> IO ()
masterMain stat mConfig = do
    let bcastport = masterBroadcastPort mConfig
        hostg = masterGlobalIP mConfig
        hostl = masterLocalIP mConfig
    ref <- liftIO $ newTVarIO stat
    let chport = show (unTCPPort (bcastport+1))
        dhpp = DHPP (T.unpack hostg,chport) (T.unpack hostl,chport)
    bracket
      (tryCreateTransport dhpp)
      closeTransport
      (\transport -> do
         node <- newLocalNode transport rtable
         runProcess node (server bcastport (pure ()) (taskManager ref))
      )


slaveMain
  :: (MasterConfig, SlaveConfig)
  -> (Gateway -> Pipeline ())     -- ^ client process
  -> IO ()
slaveMain = CloudHaskell.Client.slaveMain rtable
