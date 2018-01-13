module CloudHaskell.Server where

import           Control.Concurrent                (forkIO)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar, newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process.Lifted                  
import           Control.Monad                     (void)
import           Control.Monad.Loops               (whileJust_)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.Binary                 as Bi
import qualified Data.HashMap.Strict         as HM
import qualified Network.Simple.TCP          as NS
--
import           Network.Util

data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get

type LogProcess = ReaderT LogLock Process

tellLog msg = do
  lock <- ask
  atomicLog lock msg
                  
withHeartBeat :: ProcessId -> LogProcess ProcessId -> LogProcess ()
withHeartBeat them action = do
  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout 10000000) $ \(HB n) -> do      -- heartbeating until it fails. 
    tellLog ("heartbeat: " ++ show n)
    send them (HB n)
  tellLog "heartbeat failed: reload"                       -- when fail, it prints messages  
  kill pid "connection closed"                             -- and start over the whole process.


broadcastProcessId :: LogLock -> TMVar ProcessId -> String -> IO ()
broadcastProcessId lock pidref port = do
  NS.serve NS.HostAny port $ \(sock,addr) -> do
    atomicLog lock ("TCP connection established from " ++ show addr)
    pid <- atomically (takeTMVar pidref) 
    packAndSend sock pid


serve :: TMVar ProcessId -> LogProcess () -> LogProcess ()
serve pidref action = do
  pid <-  spawnLocal $ action >> tellLog "action finished"

  tellLog "prepartion mode"
  tellLog (show pid)
  liftIO (atomically (putTMVar pidref pid))
  tellLog "wait mode"

  local incClientNum $ serve pidref action


server :: String -> (p -> TMVar (HM.HashMap k v)  -> LogProcess ()) -> p -> Process ()
server port action p = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  resultref <- liftIO $ newTMVarIO HM.empty
  lock <- newLogLock 0 
  
  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  flip runReaderT lock $ 
    local incClientNum $ serve pidref (action p resultref)
