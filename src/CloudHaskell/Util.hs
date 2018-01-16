{-# LANGUAGE ScopedTypeVariables #-}

module CloudHaskell.Util where

import           Control.Concurrent                (forkIO,threadDelay)
import           Control.Concurrent.STM            (atomically,newTVarIO)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar,newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process       (usend)
import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node  (newLocalNode,initRemoteTable,runProcess)
import           Control.Exception                 (SomeException)
import           Control.Monad                     (void)
import           Control.Monad.Loops               (untilJust,whileJust_)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Reader
import           Data.Binary                       (Binary,Word32,decode,encode,get,put)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HM
import           Data.Typeable                     (Typeable)
import qualified Network.Simple.TCP          as NS
import           Network.Transport                 (Transport,closeTransport)
import           System.IO                         (hFlush,hPutStrLn,stderr)
--
--import           Network.Transport.TCP             (createTransport,defaultTCPParameters)
import           Network.Transport.UpHere          (createTransport
                                                   ,defaultTCPParameters
                                                   ,DualHostPortPair(..))
--
import           CloudHaskell.QueryQueue           (QQVar,emptyQQ)


recvAndUnpack :: Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (decode . BL.fromStrict) sizebstr :: Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . decode . BL.fromStrict) msg

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Word32
  in BL.toStrict (encode len)

packAndSend :: (Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . encode) x
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
                                  threadDelay (5*onesecond)
                                  return Nothing
                   Right transport -> return (Just transport)


onesecond :: Int
onesecond = 1000000


pingHeartBeat :: ProcessId -> ProcessId -> Int -> LogProcess ()
pingHeartBeat p1 them n = do
  tellLog ("heart-beat send: " ++ show n)
  send them (HB n)
  mhb <- expectTimeout (100*onesecond)
  case mhb of
    Just (HB n') -> do
      tellLog ("ping-pong received: " ++ show n')
      liftIO (threadDelay (5*onesecond))
      pingHeartBeat p1 them (n+1)
    Nothing -> do
      tellLog ("heartbeat failed!")
      liftIO (threadDelay (5*onesecond))
      kill p1 "heartbeat dead"


retrieveQueryServerPid :: LogLock
                       -> (String,Int)   -- ^ (serverid,serverport)
                       -> IO (Maybe ProcessId)
retrieveQueryServerPid lock (serverip,serverport) = do
  NS.connect serverip (show serverport) $ \(sock,addr) -> do
    atomicLog lock ("connection established to " ++ show addr)
    recvAndUnpack sock



data HeartBeat = HB { heartBeat :: Int }

instance Binary HeartBeat where
  put (HB n) = put n
  get = HB <$> get

type LogProcess = ReaderT LogLock Process


tellLog :: MonadIO m => String -> ReaderT LogLock m ()
tellLog msg = do
  lock <- ask
  atomicLog lock msg




withHeartBeat :: ProcessId -> LogProcess ProcessId -> LogProcess ()
withHeartBeat them action = do
  {- spawnLocal $ do
    let go n = do liftIO $ do
                    print n
                    threadDelay 1000000
                  send them (HB n)
                  go (n+1)
    go (0 :: Int) -}

  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout (10*onesecond)) $ \(HB n) -> do      -- heartbeating until it fails.
    tellLog ("heartbeat received: " ++ show n)
    send them (HB n)
    tellLog ("ping-pong sent: " ++ show n)
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

  tellLog "preparation mode"
  tellLog (show pid)
  liftIO (atomically (putTMVar pidref pid))
  tellLog "wait mode"
  local incClientNum $ serve pidref action


server :: QQVar k v -> String -> (p -> QQVar k v -> LogProcess ()) -> p -> Process ()
server qqvar port action p = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  lock <- newLogLock 0

  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  flip runReaderT lock $
    local incClientNum $ serve pidref (action p qqvar)


queryProcess :: forall query result a.
                (Binary query,Binary result,Typeable query,Typeable result) =>
                SendPort (query, SendPort result)
             -> query
             -> (result -> LogProcess a)
             -> LogProcess a
queryProcess sc q f = do
  (sc',rc') <- newChan :: LogProcess (SendPort result, ReceivePort result)
  sendChan sc (q,sc')
  f =<< receiveChan rc'




mainP :: forall query result.
         (Binary query, Binary result, Typeable query, Typeable result) =>
         (SendPort (query, SendPort result) -> LogProcess ())
      -> ProcessId
      -> LogProcess ()
mainP process them = do
  tellLog "mainProcess started"
  msc :: Maybe (SendPort (query,SendPort result)) <- expectTimeout 5000000
  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection established to query server"
      p1 <- spawnLocal (process sc)
      void $ pingHeartBeat p1 them 0


initP :: (ProcessId -> LogProcess ()) -> ProcessId -> LogProcess ()
initP process them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  process them


client :: (Int,String,String,String,Int) -> (ProcessId -> LogProcess ()) -> IO ()
client (portnum,hostg,hostl,serverip,serverport) process = do
  let dhpp = DHPP (hostg,show portnum) (hostl,show portnum)
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport -> do
               node <- newLocalNode transport initRemoteTable
               lock <- newLogLock 0
               emthem <- try (retrieveQueryServerPid lock (serverip,serverport))
               case emthem of
                 Left (e :: SomeException) -> do
                   atomicLog lock "exception caught"
                   atomicLog lock (show e)
                 Right mthem ->
                   case mthem of
                     Nothing -> atomicLog lock "no pid"
                     Just them -> do
                       atomicLog lock ("server id =" ++ show them)
                       runProcess node (flip runReaderT lock (process them)))
