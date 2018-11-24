{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Server where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar,takeTMVar,newEmptyTMVarIO,putTMVar)
import           Control.DeepSeq                   (NFData,deepseq)
import           Control.Distributed.Process       (ProcessId,Process
                                                   ,SendPort,ReceivePort)
import           Control.Distributed.Process.Lifted(spawnLocal,expectTimeout
                                                   ,send,receiveChan
                                                   ,newChan,sendChan,kill)
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad                     (forever,void)
import           Control.Monad.Loops               (whileJust_)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Reader.Class        (MonadReader(local))
import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import           Data.Binary                       (Binary)
import qualified Network.Simple.TCP          as NS
--
import           CloudHaskell.Socket               (packAndSend)
import           CloudHaskell.Type                 (LogLock,Pipeline,HeartBeat(..)
                                                   ,TCPPort(..)
                                                   ,Gateway(..))
import           CloudHaskell.Util                 (expectSafe,spawnChannelLocalSend
                                                   ,newLogLock,atomicLog
                                                   ,tellLog,onesecond,incClientNum)

-- | withHeartBeat makes heartbeating channel and spawn a specified process.
withHeartBeat
  :: ProcessId                  -- ^ client heart beat process
  -> (ProcessId -> Pipeline ()) -- ^ finalizer. process id is that of client's
  -> (ProcessId -> Pipeline ()) -- ^ main process
  -> Pipeline ()
withHeartBeat them_ping finalizer mainProcess = do
  tellLog "withHeartBeat start"
  (sthem_main,us_main) <- spawnChannelLocalSend $ \rthem_main -> do
    them_main <- receiveChan rthem_main
    -- NOTE: main process launch
    mainProcess them_main
  send them_ping us_main
  tellLog ("sent our main pid: " ++ show us_main)
  them_main :: ProcessId <- expectSafe
  tellLog ("got client main pid : " ++ show them_main)
  sendChan sthem_main them_main
  whileJust_ (expectTimeout (10*onesecond)) $
    \(HB n) -> do
      tellLog $ "heartbeat: " ++ show n
      -- NOTE: heartbeating until it fails.
      send them_ping (HB n)
  -- NOTE: when fail, it prints messages
  tellLog "heartbeat failed: reload"
  -- NOTE: and kill the spawned process.
  --       An enclosing process may restart the whole process.
  kill us_main "connection closed"
  finalizer them_main


-- | broadcast service information
bcastService ::
     (Binary info) =>
     LogLock      -- ^ lock for log
  -> TMVar info   -- ^ broadcasting information
  -> TCPPort      -- ^ port number
  -> IO ()
bcastService lock ref port = do
  NS.serve NS.HostAny (show (unTCPPort port)) $ \(sock,addr) -> do
    atomicLog lock ("TCP connection established from " ++ show addr)
    info <- atomically (takeTMVar ref)
    packAndSend sock info


serverUnit ::
       forall query result.
       (Serializable query, Serializable result, NFData result) =>
       ReceivePort ()
    -> (query -> Pipeline result)
    -> Pipeline ()
serverUnit lock handle = do
  -- NOTE: wait initialization
  () <- receiveChan lock
  tellLog "serverUnit started. wait for client pid"
  them <- expectSafe
  tellLog ("received client pid : " ++ show them)
  (sq :: SendPort (q,SendPort r), rq :: ReceivePort (q,SendPort r)) <- newChan
  tellLog "now we send query SendPort"
  send them sq
  -- tellLog "sent. now we wait for result SendPort"
  -- sr :: SendPort r <- expectSafe
  tellLog "receive SendPortResult, Handshake done!"
  forever $ do
    (q,sr) <- receiveChan rq
    spawnLocal $ do
      r <- handle q
      -- NOTE: result must be fully evaluated before sending.
      r `deepseq` sendChan sr r



serve :: TMVar Gateway -> Pipeline () -> Pipeline () -> Pipeline ()
serve ref web master = do
  pidWeb    <-  spawnLocal web
  pidMaster <- spawnLocal master

  tellLog $ "preparation mode: (web,master) = "  ++ show (pidWeb,pidMaster)
  liftIO $ atomically $ putTMVar ref (Gateway pidWeb pidMaster)
  tellLog "wait mode"
  -- TODO: this is an incorrect implementation. should be rewritten.
  local incClientNum $
    serve ref web master



server :: TCPPort -> Pipeline () -> Pipeline () -> Process ()
server port web master = do
  ref <- liftIO newEmptyTMVarIO
  -- liftIO $ putStrLn "server started"
  lock <- newLogLock 0

  void . liftIO $ forkIO (bcastService lock ref port)
  flip runReaderT lock $ do
    e <- runExceptT $ local incClientNum $ serve ref web master
    case e of
      Left err -> atomicLog lock (show err)
      Right _  -> pure ()
