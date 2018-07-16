{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CloudHaskell.Client where

import           Control.Concurrent                (threadDelay)
import           Control.Concurrent.STM            (atomically,retry
                                                   ,readTVar,writeTVar,modifyTVar')
import           Control.Distributed.Process       (ProcessId,SendPort,ReceivePort)
import           Control.Distributed.Process.Lifted(expectTimeout,spawnLocal
                                                   ,getSelfPid,send
                                                   ,newChan,receiveChan,sendChan
                                                   ,kill,try,bracket
                                                   )
import           Control.Distributed.Process.Node  (newLocalNode,initRemoteTable,runProcess)
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Exception                 (SomeException)
import           Control.Monad                     (forever,void)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import           Control.Monad.Trans.Except        (runExceptT)
import           Control.Monad.Trans.Reader        (ReaderT(runReaderT))
import           Data.Binary                       (Binary)
import qualified Data.IntMap                 as IM
import           Data.Text                         (Text)
import qualified Data.Text                   as T
import           Data.Typeable                     (Typeable)
import qualified Network.Simple.TCP          as NS
import           Network.Transport                 (closeTransport)
--
import           Network.Transport.UpHere          (DualHostPortPair(..))
--
import           CloudHaskell.QueryQueue           (QQVar,QueryStatus(..),next)
import           CloudHaskell.Socket               (recvAndUnpack)
import           CloudHaskell.Type                 (LogLock,Pipeline,HeartBeat(..))
import           CloudHaskell.Util                 (Router(..)
                                                   ,tellLog,atomicLog,newLogLock
                                                   ,onesecond,expectSafe
                                                   ,spawnChannelLocalReceive
                                                   ,tryCreateTransport
                                                   )

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






queryProcess :: forall query result a.
                (Binary query, Binary result, Typeable query, Typeable result) =>
                (SendPort query, ReceivePort result)
             -> query
             -> (result -> Pipeline a)
             -> Pipeline a
queryProcess (sq,rr) q f = do
  sendChan sq q
  f =<< receiveChan rr


clientUnit :: (Serializable query, Serializable result,Show query, Show result) =>
              QQVar query result
          -> (SendPort query, ReceivePort result)
          -> Pipeline ()
clientUnit qqvar (sq,rr) = do
  forever $ do
    (i,q) <- liftIO $ atomically $ do
               qq <- readTVar qqvar
               case next qq of
                 Nothing -> retry
                 Just (i,q) -> do
                   let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                   writeTVar qqvar qq'
                   return (i,q)
    tellLog ("query start: " ++ show (i,q))
    spawnLocal $ do
      r <- queryProcess (sq,rr) q return
      liftIO $ atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
      test <- liftIO $ atomically $ readTVar qqvar
      tellLog (show test)


serviceHandshake ::
     forall query result. (Serializable query, Serializable result) =>
     ProcessId
  -> ((SendPort query,ReceivePort result) ->  Pipeline ())
  -> Pipeline ()
serviceHandshake them process = do
  tellLog "service handshake process started"
  us <- getSelfPid
  tellLog ("send our pid: " ++ show us)
  send them us
  tellLog "sent. now waiting for their SendPort"
  sq :: SendPort query <- expectSafe
  tellLog "received SendPort"
  (sr :: SendPort result, rr :: ReceivePort result) <- newChan
  send them sr
  tellLog "sent SendPort"
  process (sq,rr)


routerHandshake :: (Router -> Pipeline ()) -> Pipeline ()
routerHandshake process = do
  tellLog "expecting router"
  router :: Router <- expectSafe
  tellLog "got router"
  process router


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
                 -- TODO: reorganize this cascade with ExceptT.
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
