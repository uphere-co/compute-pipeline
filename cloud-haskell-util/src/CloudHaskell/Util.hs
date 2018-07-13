{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Util where

import           Control.Concurrent                (forkIO,threadDelay)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TChan      (readTChan,writeTChan,newTChanIO)
import           Control.Concurrent.STM.TMVar      ( TMVar
                                                   , takeTMVar,newTMVarIO
                                                   , newEmptyTMVarIO, putTMVar)
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
import           Control.Monad.Reader.Class        (MonadReader(ask,local))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (ExceptT(..),runExceptT)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import           Data.Binary                       (Binary,Word32,decode,encode)
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
import           CloudHaskell.Type                 (LogLock,Pipeline,PipelineError(..)
                                                   ,HeartBeat(..))

expectSafe :: forall a. (Binary a, Typeable a) => Pipeline a
expectSafe = ExceptT $ lift $ receiveWait [matchAny f]
  where
    f msg = do
      case messageFingerprint msg == fingerprint (undefined :: a) of
        False -> pure (Left (PipelineError "fingerprint mismatch"))
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


pingHeartBeat :: [ProcessId] -> ProcessId -> Int -> Pipeline ()
pingHeartBeat ps them n = do
  tellLog ("heart-beat send: " ++ show n)
  send them (HB n)
  mhb <- expectTimeout (10*onesecond)
  case mhb of
    Just (HB _n') -> do
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


tellLog :: (MonadReader LogLock m, MonadIO m) => String -> m ()
tellLog msg = do
  lock <- ask
  atomicLog lock msg


withHeartBeat :: ProcessId -> (ProcessId -> Pipeline ()) -> Pipeline ()
withHeartBeat them_ping action = do
  chan <- liftIO newTChanIO
  us_main <- spawnLocal $ do
    them_main <- liftIO $ atomically $ readTChan chan
    action them_main                           -- main process launch
  send them_ping us_main
  tellLog ("sent our main pid: " ++ show us_main)
  them_main :: ProcessId <- expectSafe
  tellLog ("got client main pid : " ++ show them_main)
  liftIO $ atomically $ writeTChan chan them_main
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


server :: queue -> String -> (state -> queue -> Pipeline ()) -> state -> Process ()
server queue port action state = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  lock <- newLogLock 0

  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  flip runReaderT lock $ do
    e <- runExceptT $ local incClientNum $ serve pidref (action state queue)
    case e of
      Left err -> atomicLog lock (show err)
      Right _  -> pure ()


queryProcess :: forall query result a.
                (Binary query, Binary result, Typeable query, Typeable result) =>
                (SendPort query, ReceivePort result)
             -> query
             -> (result -> Pipeline a)
             -> Pipeline a
queryProcess (sq,rr) q f = do
  sendChan sq q
  f =<< receiveChan rr


mainP :: forall query result.
         (Binary query, Binary result, Typeable query, Typeable result) =>
         ((SendPort query,ReceivePort result) ->  Pipeline ())
      -> Pipeline ()
mainP process = do
  tellLog "start mainProcess"
  them :: ProcessId <- expectSafe
  tellLog "connected"
  sq :: SendPort query <- expectSafe
  tellLog "received SendPort"
  (sr :: SendPort result, rr :: ReceivePort result) <- newChan
  send them sr
  tellLog "sent SendPort"
  process (sq,rr)


heartBeatHandshake :: ProcessId -> (ProcessId -> Pipeline ()) -> Pipeline ()
heartBeatHandshake them_ping process = do
  us_ping <- getSelfPid
  tellLog ("our ping process is " ++ show us_ping)
  send them_ping us_ping
  tellLog ("out ping pid is sent")
  them :: ProcessId <- expectSafe
  tellLog ("got their pid " ++ show them)
  lock <- liftIO newEmptyTMVarIO
  p1 <- spawnLocal $ do
    us_main <- getSelfPid
    send them_ping us_main
    tellLog ("sent our process id " ++ show us_main)
    liftIO $ atomically (putTMVar lock ())
    process them
  void $ liftIO $ atomically $ takeTMVar lock
  void $ pingHeartBeat [p1] them_ping 0


client :: (Int,Text,Text,Text,Int) -> (ProcessId -> Pipeline ()) -> IO ()
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
                         runProcess node $ flip runReaderT lock $ do
                           e <- runExceptT (process them)
                           case e of
                             Left err -> atomicLog lock (show err)
                             Right _  -> pure ()
                 threadDelay (5*onesecond))
