{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Util where

import           Control.Concurrent                (forkIO,threadDelay)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      ( TMVar
                                                   , takeTMVar,newTMVarIO
                                                   , newEmptyTMVarIO, putTMVar)
-- import           Control.DeepSeq                   (NFData,deepseq)
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
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT),ask,local)
import           Data.Binary                       (Binary,Word32,decode,encode,get,put)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                         (Text)
import qualified Data.Text                   as T
import           Data.Typeable                     (Typeable)
import           GHC.Generics                      (Generic)
import qualified Network.Simple.TCP          as NS
import           Network.Transport                 (Transport,closeTransport)
import           System.IO                         (hFlush,hPutStrLn,stderr)
import           Unsafe.Coerce
--
import           Network.Transport.UpHere          (createTransport
                                                   ,defaultTCPParameters
                                                   ,DualHostPortPair(..))


expectSafe :: forall a. (Binary a, Typeable a) => Process (Either String a)
expectSafe = receiveWait [matchAny f]
  where
    f msg = do
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
  pid <- action                                -- main process launch
  whileJust_ (expectTimeout (10*onesecond)) $
    \(HB n) -> send them (HB n)                -- heartbeating until it fails.
  tellLog "heartbeat failed: reload"           -- when fail, it prints messages
  kill pid "connection closed"                 -- and start over the whole process.


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


server :: queue -> String -> (state -> queue -> LogProcess ()) -> state -> Process ()
server queue port action state = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  lock <- newLogLock 0

  void . liftIO $ forkIO (broadcastProcessId lock pidref port)
  flip runReaderT lock $
    local incClientNum $ serve pidref (action state queue)


queryProcess :: forall query result a.
                (Binary query,Binary result,Typeable query,Typeable result) =>
                (SendPort query, ReceivePort result)
             -> query
             -> (result -> LogProcess a)
             -> LogProcess a
queryProcess (sq,rr) q f = do
  sendChan sq q
  f =<< receiveChan rr


mainP :: forall query result.
         (Binary query, Binary result, Typeable query, Typeable result) =>
         ((SendPort query,ReceivePort result) ->  LogProcess ())
      -> ProcessId
      -> LogProcess ()
mainP process them_ping = do
  tellLog "start mainProcess"
  esq :: Either String (ProcessId,SendPort query) <- lift expectSafe
  case esq of
    Left err -> tellLog err
    Right (them,sq) -> do
      tellLog "connected: received SendPort"
      liftIO $ threadDelay 1000000
      (sr :: SendPort result, rr :: ReceivePort result) <- newChan
      send them sr
      tellLog "sent SendPort"
      p1 <- spawnLocal (process (sq,rr))
      void $ pingHeartBeat [p1] them_ping 0


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


data Q = Q deriving (Show,Generic)

instance Binary Q

data R = R deriving (Show,Generic)

instance Binary R
