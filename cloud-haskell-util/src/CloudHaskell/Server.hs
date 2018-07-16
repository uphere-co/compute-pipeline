{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Server where

import           Control.Concurrent                 (forkIO)
import           Control.Concurrent.STM             (atomically)
import           Control.Concurrent.STM.TMVar       (TMVar,takeTMVar,newEmptyTMVarIO,putTMVar)
import           Control.Distributed.Process        (ProcessId,Process)
import           Control.Distributed.Process.Lifted (spawnLocal,expectTimeout
                                                    ,send,receiveChan,sendChan,kill)
import           Control.Monad                     (void)
import           Control.Monad.Loops               (whileJust_)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Reader.Class        (MonadReader(local))
import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import qualified Network.Simple.TCP          as NS
--
import           CloudHaskell.Socket               (packAndSend)
import           CloudHaskell.Type                 (LogLock,Pipeline,HeartBeat(..))

import           CloudHaskell.Util                 (expectSafe,spawnChannelLocalSend
                                                   ,newLogLock,atomicLog
                                                   ,tellLog,onesecond,incClientNum)

withHeartBeat :: ProcessId -> (ProcessId -> Pipeline ()) -> Pipeline ()
withHeartBeat them_ping action = do
  (sthem_main,us_main) <- spawnChannelLocalSend $ \rthem_main -> do
    them_main <- receiveChan rthem_main
    action them_main                           -- main process launch
  send them_ping us_main
  tellLog ("sent our main pid: " ++ show us_main)
  them_main :: ProcessId <- expectSafe
  tellLog ("got client main pid : " ++ show them_main)
  sendChan sthem_main them_main
  whileJust_ (expectTimeout (10*onesecond)) $
    \(HB n) -> do
      tellLog $ "heartbeat: " ++ show n
      send them_ping (HB n)                -- heartbeating until it fails.
  tellLog "heartbeat failed: reload"           -- when fail, it prints messages
  kill us_main "connection closed"                 -- and start over the whole process.


broadcastProcessId :: LogLock -> TMVar ProcessId -> String -> IO ()
broadcastProcessId lock pidref port = do
  NS.serve NS.HostAny port $ \(sock,addr) -> do
    atomicLog lock ("TCP connection established from " ++ show addr)
    pid <- atomically (takeTMVar pidref)
    packAndSend sock pid


serve :: TMVar ProcessId -> Pipeline () -> Pipeline ()
serve pidref action = do
  pid <-  spawnLocal $ action >> tellLog "action finished"

  tellLog "preparation mode"
  tellLog (show pid)
  liftIO (atomically (putTMVar pidref pid))
  tellLog "wait mode"
  local incClientNum $ serve pidref action


server :: String -> Pipeline () -> Process ()
server port action = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  lock <- newLogLock 0

  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  flip runReaderT lock $ do
    e <- runExceptT $ local incClientNum $ serve pidref action
    case e of
      Left err -> atomicLog lock (show err)
      Right _  -> pure ()
