{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Util where

import           Control.Concurrent                (threadDelay)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TMVar      (takeTMVar,newTMVarIO,putTMVar)
import           Control.Distributed.Process (ProcessId,SendPort,ReceivePort)
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
import           Control.Monad.Loops               (untilJust)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Reader.Class        (MonadReader(ask))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (ExceptT(..),runExceptT)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import           Data.Binary                       (Binary,decode)
import qualified Data.ByteString.Char8       as BC
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
import           CloudHaskell.Socket               (recvAndUnpack)
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

spawnChannelLocalSend :: Serializable a => (ReceivePort a -> Pipeline ()) -> Pipeline (SendPort a, ProcessId)
spawnChannelLocalSend process = do
  (schan,rchan) <- newChan
  pid <- spawnLocal (process rchan)
  pure (schan, pid)

spawnChannelLocalReceive :: Serializable a => (SendPort a -> Pipeline ()) -> Pipeline (ReceivePort a, ProcessId)
spawnChannelLocalReceive process = do
  (schan,rchan) <- newChan
  pid <- spawnLocal (process schan)
  pure (rchan, pid)











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


heartBeatHandshake :: ProcessId -> Pipeline () -> Pipeline ()
heartBeatHandshake them_ping main = do
  us_ping <- getSelfPid
  tellLog ("our ping process is " ++ show us_ping)
  send them_ping us_ping
  tellLog ("out ping pid is sent")
  them :: ProcessId <- expectSafe
  tellLog ("got their pid " ++ show them)
  (rchan,p1) <- spawnChannelLocalReceive $ \schan -> do
    us_main <- getSelfPid
    send them_ping us_main
    tellLog ("sent our process id " ++ show us_main)
    sendChan schan ()
    main
  _ <- receiveChan rchan
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
