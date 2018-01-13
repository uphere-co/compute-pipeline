module CloudHaskell.Util where

import           Control.Concurrent                (forkIO,threadDelay)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar, newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process.Lifted                  
import           Control.Monad                     (void)
import           Control.Monad.Loops               (untilJust,whileJust_)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Trans.Reader
import qualified Data.Binary                 as Bi
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HM
import qualified Network.Simple.TCP          as NS
import           Network.Transport                 (Transport)
import           System.IO                         (hFlush,hPutStrLn,stderr)
--
import           Network.Transport.UpHere          (createTransport
                                                   ,defaultTCPParameters
                                                   ,DualHostPortPair(..))



-- import           Network.Util



recvAndUnpack :: Bi.Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (Bi.decode . BL.fromStrict) sizebstr :: Bi.Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . Bi.decode . BL.fromStrict) msg

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Bi.Word32
  in BL.toStrict (Bi.encode len)

packAndSend :: (Bi.Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . Bi.encode) x
      sizebstr = packNumBytes msg
  NS.send sock sizebstr
  NS.send sock msg



type LogLock = (TMVar (),Int)

newLogLock :: (MonadIO m) => Int -> m LogLock
newLogLock n = liftIO $ (,) <$> newTMVarIO () <*> pure n

atomicLog :: (MonadIO m) => LogLock -> String -> m ()
atomicLog lock str = liftIO $ do
  let n = snd lock
  atomically $ takeTMVar (fst lock)
  let result = BC.pack ("[" ++ show n ++ "]: " ++ str)
  result `seq` BC.hPutStrLn stderr result
  hFlush stderr
  atomically $ putTMVar (fst lock) ()

getClientNum :: LogLock -> Int
getClientNum (_l,n) = n

incClientNum :: LogLock -> LogLock
incClientNum (l,n) = (l,n+1)

tryCreateTransport :: DualHostPortPair -> IO Transport
tryCreateTransport dhpp =
  untilJust $ do etransport <- createTransport dhpp defaultTCPParameters
                 case etransport of
                   Left err -> do hPutStrLn stderr (show err)
                                  threadDelay 5000000
                                  return Nothing
                   Right transport -> return (Just transport)


pingHeartBeat :: ProcessId -> ProcessId -> Int -> LogProcess ()
pingHeartBeat p1 them n = do
  send them (HB n)
  liftIO (threadDelay 5000000)
  mhb <- expectTimeout 10000000
  case mhb of
    Just (HB n') -> do
      tellLog ("ping-pong : " ++ show n')
      pingHeartBeat p1 them (n+1)
    Nothing -> do
      tellLog ("heartbeat failed!")
      kill p1 "heartbeat dead"


retrieveQueryServerPid :: LogLock
                       -> (String,Int)   -- ^ (serverid,serverport)
                       -> IO (Maybe ProcessId)
retrieveQueryServerPid lock (serverip,serverport) = do
  NS.connect serverip (show serverport) $ \(sock,addr) -> do
    atomicLog lock ("connection established to " ++ show addr)
    recvAndUnpack sock



data HeartBeat = HB { heartBeat :: Int }

instance Bi.Binary HeartBeat where
  put (HB n) = Bi.put n
  get = HB <$> Bi.get

type LogProcess = ReaderT LogLock Process


tellLog :: MonadIO m => String -> ReaderT LogLock m ()
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
