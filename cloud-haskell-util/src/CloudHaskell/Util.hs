{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CloudHaskell.Util where

import           Control.Concurrent                (forkOS,threadDelay)
import           Control.Concurrent.STM            (atomically,newTVarIO)
import           Control.Concurrent.STM.TMVar      (takeTMVar,newTMVarIO,putTMVar)
import           Control.Error.Safe                (justErr)
import           Control.Distributed.Process       (ProcessId,SendPort,ReceivePort)
import           Control.Distributed.Process.Internal.CQueue ()
import           Control.Distributed.Process.Internal.Primitives (matchAny,receiveWait)
import           Control.Distributed.Process.Internal.Types (Message(..))
import           Control.Distributed.Process.Lifted(spawnLocal,newChan,sendChan,receiveChan)
import           Control.Distributed.Process.Lifted.Class (MonadProcessBase)
import           Control.Distributed.Process.Serializable
import           Control.Monad                     (forever,void)
import           Control.Monad.Loops               (untilJust)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Reader.Class        (MonadReader(ask))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (ExceptT(..))
import           Data.Binary                       (Binary,decode)
import qualified Data.ByteString.Char8       as BC
-- import           Data.IntMap                       (IntMap)
-- import qualified Data.IntMap                 as IM
import qualified Data.HashMap.Strict         as HM
import           Data.Text                         (Text)
import qualified Data.Text                   as T
import           Data.Typeable                     (Typeable)
import           Network.Transport                 (Transport)
import           System.IO                         (hFlush,hPutStrLn,stderr)
import           Unsafe.Coerce
--
import           Network.Transport.UpHere          (createTransport
                                                   ,defaultTCPParameters
                                                   ,DualHostPortPair(..))
--
import           CloudHaskell.QueryQueue           (QQVar,singleQuery,emptyQQ)
import           CloudHaskell.Type                 (LogLock,Pipeline,PipelineError(..)
                                                   ,Router(..))


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



tellLog :: (MonadReader LogLock m, MonadIO m) => String -> m ()
tellLog msg = do
  lock <- ask
  atomicLog lock msg

spawnChannelLocalSend ::
       (Serializable a, MonadProcessBase m) =>
       (ReceivePort a -> m ())
    -> m (SendPort a, ProcessId)
spawnChannelLocalSend process = do
  (schan,rchan) <- newChan
  pid <- spawnLocal (process rchan)
  pure (schan, pid)

spawnChannelLocalReceive ::
       (Serializable a, MonadProcessBase m) =>
       (SendPort a -> m ())
    -> m (ReceivePort a, ProcessId)
spawnChannelLocalReceive process = do
  (schan,rchan) <- newChan
  pid <- spawnLocal (process schan)
  pure (rchan, pid)


type RequestDuplex q r = (SendPort q, ReceivePort r)
type RespondDuplex q r = (ReceivePort q, SendPort r)

spawnChannelLocalDuplex ::
      (Serializable q,Serializable r, MonadProcessBase m) =>
      (RespondDuplex q r -> m ())
   -> m (RequestDuplex q r, ProcessId)
spawnChannelLocalDuplex process = do
  (sq,rq) <- newChan
  (sr,rr) <- newChan
  pid <- spawnLocal $ process (rq,sr)
  pure ((sq,rr),pid)


ioWorker ::
       (Serializable q, Serializable r, MonadProcessBase m) =>
       (ReceivePort q, SendPort r)
    -> (QQVar q r -> IO ())
    -> m ()
ioWorker (rq,sr) daemon = do
  qqvar <- liftIO (newTVarIO emptyQQ)
  void $ liftIO $ forkOS $ daemon qqvar
  forever $ do
    q <- receiveChan rq
    r <- liftIO $ singleQuery qqvar q
    sendChan sr r
    liftIO $ putStrLn "query served"


lookupRouter :: Text -> Router -> Pipeline ProcessId
lookupRouter key router =
  ExceptT $ pure $
    justErr (RouteError ("no such route:" ++ T.unpack key)) $
      HM.lookup key (unRouter router)
