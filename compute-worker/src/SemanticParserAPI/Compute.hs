{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemanticParserAPI.Compute where

import           Control.Concurrent.STM                    (TVar,atomically
                                                           ,newTVarIO,readTVar,writeTVar)
import           Control.Distributed.Process               (Process,processNodeId)
import           Control.Distributed.Process.Lifted        (ProcessId
                                                           ,expect,getSelfPid,send
                                                           ,newChan,receiveChan
                                                           ,spawnLocal)
import           Control.Distributed.Process.Node          (newLocalNode,runProcess)
import           Control.Exception                         (bracket)
import           Control.Lens                              ((&),(.~),(^.),(%~),at)
import           Control.Monad                             (forever)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T  (unpack)
import           Network.Transport                         (closeTransport)
-- compute-pipeline
import           CloudHaskell.Closure                      ((@<),spawnChannel_)
import           CloudHaskell.Server                       (server,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,TCPPort(..),Router(..))
import           CloudHaskell.Util                         (RequestDuplex
                                                           ,tellLog
                                                           ,expectSafe
                                                           ,ioWorker
                                                           ,tryCreateTransport
                                                           ,spawnChannelLocalDuplex
                                                           )
import           Network.Transport.UpHere                  (DualHostPortPair(..))
import           Task.CoreNLP (QCoreNLP(..),RCoreNLP(..))
--this package
import           SemanticParserAPI.Compute.Handler         (requestHandler)
import           SemanticParserAPI.Compute.Task            (rtable
                                                           ,remoteDaemonCoreNLP__closure)
import           SemanticParserAPI.Compute.Type.Status     (NodeStatus(..)
                                                           ,nodeStatusMainProcessId
                                                           ,nodeStatusIsServing
                                                           ,Status
                                                           ,statusNodes)
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)


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
    let nstat = NodeStatus pid False duplex
        m' = m & (statusNodes . at cname .~ Just (Just nstat))
    writeTVar ref m'

updateLinkedProcessStatus :: TVar Status -> (Text,Bool) -> Pipeline ()
updateLinkedProcessStatus ref (cname,status) =
  liftIO $ atomically $ do
    m <- readTVar ref
    let mnstat = m ^. statusNodes . at cname
    case mnstat of
      Just (Just nstat) -> do
        let nstat' = nstat & nodeStatusIsServing .~ status
            m' = m & (statusNodes . at cname .~ Just (Just nstat'))
        writeTVar ref m'
      _ -> pure ()

launchTask :: TVar Status -> Text -> ProcessId -> Pipeline ()
launchTask ref cname pid = do
  let nid = processNodeId pid
  tellLog $ "node id = " ++ show nid
  (sr,rr) <- newChan
  (sstat,rstat) <- newChan
  sq <- spawnChannel_ nid (remoteDaemonCoreNLP__closure @< sstat @< sr)
  -- for monitoring
  spawnLocal $ forever $ do
    b <- receiveChan rstat
    updateLinkedProcessStatus ref (cname,b)
  let duplex = (sq,rr)
  addLinkedProcess ref (cname,pid,duplex)


taskManager :: TVar Status -> Pipeline ()
taskManager ref = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (elimLinkedProcess ref) $ \them_main -> do
    themaster <- getSelfPid
    let router = Router $ HM.insert "master" themaster mempty
    send them_main router
    cname :: Text <- expectSafe
    tellLog $ "taskManager: got " ++ show cname
    launchTask ref cname them_main
    () <- expect  -- for idling
    pure ()

initDaemonAndServer :: TVar Status -> TCPPort -> (Bool,Bool) -> FilePath -> Process ()
initDaemonAndServer ref port (bypassNER,bypassTEXTNER) lcfg = do
  ((sq,rr),_) <- spawnChannelLocalDuplex $ \(rq,sr) ->
    ioWorker (rq,sr) (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)
  server port (requestHandler ref (sq,rr)) (taskManager ref) -- (requestHandler2 ref)


computeMain :: Status
            -> (TCPPort,Text,Text)
            -> (Bool,Bool)  -- ^ (bypassNER, bypassTEXTNER)
            -> FilePath -- ^ configjson "/home/wavewave/repo/srcp/lexicon-builder/config.json.mark"
            -> IO ()
computeMain stat (bcastport,hostg,hostl) (bypassNER,bypassTEXTNER) lcfg = do
    ref <- liftIO $ newTVarIO stat
    let chport = show (unTCPPort (bcastport+1))
        dhpp = DHPP (T.unpack hostg,chport) (T.unpack hostl,chport)
    bracket
            (tryCreateTransport dhpp)
            closeTransport
            (\transport ->
                    newLocalNode transport rtable
                >>= \node -> runProcess node
                               (initDaemonAndServer ref bcastport (bypassNER,bypassTEXTNER) lcfg)
            )
