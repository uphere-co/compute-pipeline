{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.CLI.Client where

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process.Lifted  (Process,ProcessId
                                                     ,expectTimeout,getSelfPid,kill,send,)
import           Control.Distributed.Process.Node
import           Control.Exception                   (SomeException(..),bracket,try)

import           Control.Monad                       (void,join)
import           Control.Monad.IO.Class              (liftIO)
-- import           Control.Monad.Loops
-- import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Reader          (runReaderT)
-- import qualified Data.ByteString.Lazy.Char8         as BL
-- import qualified Data.Text                          as T
import qualified Network.Simple.TCP                 as NS
-- import           System.Console.Haskeline                 (runInputT,getInputLine,defaultSettings)
import           System.Console.Haskeline.MonadException (MonadException(controlIO),RunIO(..))
--
import           Network.Transport                   (closeTransport)
import           Network.Transport.UpHere            (DualHostPortPair(..))
--
import           CloudHaskell.Server
import           Network.Util                        (LogLock,newLogLock
                                                     ,atomicLog,recvAndUnpack
                                                     ,tryCreateTransport)



instance MonadException Process where
  controlIO f = join . liftIO $ f (RunIO return)



initProcess :: ProcessId -> LogProcess ()
initProcess them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  void (mainProcess them)

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


{-
consoleClient :: SendPort (Query, SendPort BL.ByteString) -> LogProcess ()
consoleClient sc = do
  runInputT defaultSettings $
    whileJust_ (getInputLine "% ") $ \input' -> do
      lift $ queryProcess sc (QueryText (T.pack input') []) (liftIO . BL.putStrLn)

-}

mainProcess :: ProcessId -> LogProcess ()
mainProcess _them = do
  tellLog "mainProcess started"
  {-
  msc :: Maybe (SendPort (Query,SendPort BL.ByteString)) <- expectTimeout 5000000
  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection stablished to query server"
      -- lock <- ask
      p1 <- spawnLocal (consoleClient sc)
      void $ pingHeartBeat p1 them 0
  -}

{-
queryProcess :: SendPort (Query, SendPort BL.ByteString) -> Query -> (BL.ByteString -> LogProcess a) -> LogProcess a
queryProcess sc q f = do
  (sc',rc') <- newChan :: LogProcess (SendPort BL.ByteString, ReceivePort BL.ByteString)
  sendChan sc (q,sc')
  f =<< receiveChan rc'


-}


clientMain :: (Int,String,String,String,Int) -> IO ()
clientMain (portnum,hostg,hostl,serverip,serverport) = do
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
                       atomicLog lock (show them)
                       runProcess node (flip runReaderT lock (initProcess them)))

