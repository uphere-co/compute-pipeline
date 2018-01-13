{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticParserAPI.CLI.Client where

import           Control.Concurrent                  (threadDelay)
import           Control.Distributed.Process.Lifted  (Process,ProcessId,SendPort,ReceivePort
                                                     ,expectTimeout
                                                     ,getSelfPid
                                                     ,newChan,receiveChan,sendChan
                                                     ,send,spawnLocal)
import           Control.Distributed.Process.Node
import           Control.Exception                   (SomeException(..),bracket,try)

import           Control.Monad                       (forever,void,join)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Loops                 (whileJust_)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Reader          (runReaderT)
import           Data.Binary                         (Binary)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import           Data.Typeable                       (Typeable)
import           System.Console.Haskeline            (runInputT,getInputLine,defaultSettings)
import           System.Console.Haskeline.MonadException (MonadException(controlIO),RunIO(..))
--
import           Network.Transport                   (closeTransport)
import           Network.Transport.UpHere            (DualHostPortPair(..))
--
import           CloudHaskell.Util                   (LogProcess,newLogLock
                                                     ,atomicLog,tellLog
                                                     ,tryCreateTransport
                                                     ,pingHeartBeat
                                                     ,retrieveQueryServerPid
                                                     )
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..))



instance MonadException Process where
  controlIO f = join . liftIO $ f (RunIO return)






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




consoleClient :: SendPort (ComputeQuery, SendPort ComputeResult) -> LogProcess ()
consoleClient sc = do
  runInputT defaultSettings $
    whileJust_ (getInputLine "% ") $ \input' -> do
      -- liftIO $ print input'
      lift $ queryProcess sc (CQ_Text (T.pack input')) (liftIO . print)



mainProcess :: ProcessId -> LogProcess ()
mainProcess them = do
  tellLog "mainProcess started"
  msc :: Maybe (SendPort (ComputeQuery,SendPort ComputeResult)) <- expectTimeout 5000000
  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection stablished to query server"
      p1 <- spawnLocal (consoleClient sc)
      void $ pingHeartBeat p1 them 0


initProcess :: ProcessId -> LogProcess ()
initProcess them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  void (mainProcess them)


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

