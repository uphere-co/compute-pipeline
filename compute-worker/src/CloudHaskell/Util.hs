{-# LANGUAGE ScopedTypeVariables #-}

module CloudHaskell.Util where

import           Control.Concurrent                (forkIO,threadDelay)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (TMVar, takeTMVar,newTMVarIO, newEmptyTMVarIO, putTMVar)
import           Control.Distributed.Process (ProcessId,SendPort,ReceivePort,Process)
import           Control.Distributed.Process.Internal.CQueue ()
import           Control.Distributed.Process.Internal.Primitives (matchAny,receiveWait)
import           Control.Distributed.Process.Internal.Types (Message(..))
import           Control.Distributed.Process.Lifted (spawnLocal,expectTimeout
                                                    ,getSelfPid,send
                                                    ,newChan,receiveChan,sendChan
                                                    ,kill,try,bracket
                                                    )
import           Control.Distributed.Process.Node  (newLocalNode,initRemoteTable,runProcess)
import           Control.Distributed.Process.Serializable
import           Control.Exception                 (SomeException)
import           Control.Monad                     (forever,void)
import           Control.Monad.Loops               (untilJust,whileJust_)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Trans.Reader
import           Data.Binary                       (Binary,Word32,decode,encode,get,put)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                         (Text)
import qualified Data.Text                   as T
import           Data.Typeable                     (Typeable)
import qualified Network.Simple.TCP          as NS
import           Network.Transport                 (Transport,closeTransport)
import           System.IO                         (hFlush,hPutStrLn,stderr)
import           Unsafe.Coerce
--
import           Network.Transport.UpHere          (createTransport
                                                   ,defaultTCPParameters
                                                   ,DualHostPortPair(..))
--
import           CloudHaskell.QueryQueue           (QQVar)


expectSafe :: forall a. (Serializable a) => Process (Either String a)
expectSafe = receiveWait [matchAny f]
  where
    f msg = do
      -- liftIO $ print (messageFingerprint msg)
      -- liftIO $ print (fingerprint (undefined :: a))
      case messageFingerprint msg == fingerprint (undefined :: a) of
        False -> pure (Left "fingerprint mismatch")
        True ->
          case msg of
            (UnencodedMessage _ m) ->
              let m' = unsafeCoerce m :: a in pure (Right m')
            (EncodedMessage _ _) -> pure (Right decoded)
              where
                decoded :: a
                !decoded = decode (messageEncoding msg)

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


pingHeartBeat :: [ProcessId] -> ProcessId -> Int -> LogProcess ()
pingHeartBeat ps them n = do
  -- tellLog ("heart-beat send: " ++ show n)
  send them (HB n)
  mhb <- expectTimeout (10*onesecond)
  case mhb of
    Just (HB n') -> do
      -- tellLog ("ping-pong received: " ++ show n')
      liftIO (threadDelay (5*onesecond))
      pingHeartBeat ps them (n+1)
    Nothing -> do
      tellLog ("heartbeat failed!")
      liftIO (threadDelay (5*onesecond))
      mapM_ (flip kill "heartbeat dead") ps


retrieveQueryServerPid :: LogLock
                       -> (Text,Int)   -- ^ (serverid,serverport)
                       -> IO (Maybe ProcessId)
retrieveQueryServerPid lock (serverip,serverport) = do
  NS.connect (T.unpack serverip) (show serverport) $ \(sock,addr) -> do
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
  pid <- action                                            -- main process launch
  whileJust_ (expectTimeout (10*onesecond)) $ \(HB n) -> do      -- heartbeating until it fails.
    -- tellLog ("heartbeat received: " ++ show n)
    send them (HB n)
    -- tellLog ("ping-pong sent: " ++ show n)
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
  tellLog "start mainProcess"
  msc :: Maybe (SendPort (query,SendPort result)) <- expectTimeout 5000000
  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection established to query server"
      p1 <- spawnLocal (process sc)
      void $ pingHeartBeat [p1] them 0


initP :: (ProcessId -> LogProcess ()) -> ProcessId -> LogProcess ()
initP process them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  process them


client :: (Int,Text,Text,Text,Int) -> (ProcessId -> LogProcess ()) -> IO ()
client (portnum,hostg,hostl,serverip,serverport) process = do
  let dhpp = DHPP (T.unpack hostg,show portnum) (T.unpack hostl,show portnum)
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport -> do
               node <- newLocalNode transport initRemoteTable
               lock <- newLogLock 0
               forever $ do
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
                         runProcess node (flip runReaderT lock (process them))
                 threadDelay (5*onesecond))
