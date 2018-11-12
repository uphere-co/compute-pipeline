{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CloudHaskell.Util where

import           Control.Concurrent                ( forkOS, threadDelay )
import           Control.Concurrent.STM            ( atomically, newTVarIO )
import           Control.Concurrent.STM.TMVar      ( takeTMVar, newTMVarIO, putTMVar )
import           Control.Error.Safe                ( justErr )
import           Control.Distributed.Process       ( Process, ProcessId
                                                   , SendPort, ReceivePort
                                                   )
import           Control.Distributed.Process.Internal.CQueue ()
import           Control.Distributed.Process.Internal.Primitives ( matchAny, receiveWait )
import           Control.Distributed.Process.Internal.Types ( Message(..) )
import           Control.Distributed.Process.Lifted( spawnLocal
                                                   , newChan, sendChan, receiveChan
                                                   )
import           Control.Distributed.Process.Lifted.Class ( MonadProcessBase )
import           Control.Distributed.Process.Serializable
import           Control.Monad                     ( forever, void )
import           Control.Monad.Loops               ( untilJust )
import           Control.Monad.IO.Class            ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class        ( MonadReader(ask) )
import           Control.Monad.Trans.Class         ( lift )
import           Control.Monad.Trans.Except        ( ExceptT(..), runExceptT )
import           Control.Monad.Trans.Reader        ( ReaderT )
import           Data.Binary                       ( Binary, decode )
import qualified Data.ByteString.Char8       as BC
import qualified Data.HashMap.Strict         as HM
import           Data.Text                         ( Text )
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Typeable                     ( Typeable )
import           Network.Transport                 ( Transport )
import           System.IO                         ( hFlush, hPutStrLn, stderr )
import           Unsafe.Coerce
--
import           Network.Transport.UpHere          ( createTransport
                                                   , defaultTCPParameters
                                                   , DualHostPortPair(..)
                                                   )
--
import           CloudHaskell.QueryQueue           ( QQVar, singleQuery, emptyQQ )
import           CloudHaskell.Type                 ( LogLock, Pipeline, PipelineError(..)
                                                   , RenderError(..), Router(..)
                                                   )


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


atomicLogText :: (MonadIO m) => LogLock -> Text -> m ()
atomicLogText lock txt = liftIO $ do
  let n = snd lock
  atomically $ takeTMVar (fst lock)
  let result = "[" <> T.pack (show n) <> "]: " <> txt
  result `seq` TIO.hPutStrLn stderr result
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



-- | Simplest handling errors by just logging on stderr.
handleError :: (RenderError e) => ExceptT e IO a -> IO ()
handleError m = do
  r <- runExceptT m
  case r of
    Left e -> TIO.hPutStrLn stderr (renderError e)
    Right _ -> pure ()


-- | Simplest handling errors by logging on stderr with atomic lock.
handleErrorLog ::
     (RenderError e)
  => ExceptT e (ReaderT LogLock Process) a
  -> ReaderT LogLock Process ()
handleErrorLog m = do
  r <- runExceptT m
  case r of
    Left e -> ask >>= \lock -> atomicLogText lock (renderError e)
    Right _ -> pure ()
