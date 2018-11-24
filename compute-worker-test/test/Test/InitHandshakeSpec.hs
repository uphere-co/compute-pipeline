{-# LANGUAGE TypeApplications #-}
module Test.InitHandshakeSpec where

import Control.Concurrent          ( MVar
                                   , forkIO
                                   , newEmptyMVar
                                   , takeMVar
                                   , putMVar
                                   )
import Control.Distributed.Process.Lifted
                                   ( ProcessId
                                   , getSelfPid
                                   , spawnChannelLocal
                                   , spawnLocal
                                   , sendChan
                                   , receiveChan
                                   )
import Control.Distributed.Process.Node.Lifted
                                   ( initRemoteTable
                                   , newLocalNode
                                   , runProcess
                                   )
import Control.Monad               ( void )
import Control.Monad.IO.Class      ( liftIO )
import Control.Monad.Trans.Except  ( runExceptT )
import Control.Monad.Trans.Reader  ( runReaderT )
import Network.Transport           ( Transport, closeTransport )
import Network.Transport.TCP       ( createTransport
                                   , defaultTCPParameters
                                   )
import Test.Hspec
------
import CloudHaskell.Client         ( heartBeatHandshake )
import CloudHaskell.Server         ( withHeartBeat )
import CloudHaskell.Type           ( Pipeline )
import CloudHaskell.Util           ( expectSafe, newLogLock )


withTransport :: (Transport -> IO a) -> IO a
withTransport action = do
  let host = "127.0.0.1"
  etrnsprt <- createTransport host "12345" (\port' -> (host,port')) defaultTCPParameters
  case etrnsprt of
    Left e ->  fail (show e)
    Right transport -> action transport >>= \r -> closeTransport transport >> pure r


testHandshake :: (MVar (),MVar ()) -> Pipeline ()
testHandshake (lock_server,lock_client) = do
  server_ping <- getSelfPid
  spawnLocal (client lock_client server_ping)
  server lock_server


server :: MVar () -> Pipeline ()
server lock_server = do
  client_ping <- expectSafe @ProcessId
  withHeartBeat client_ping (\_ -> pure ()) $ \_ ->
    liftIO $ putMVar lock_server ()


client :: MVar () -> ProcessId -> Pipeline ()
client lock_client server_ping =
  heartBeatHandshake server_ping $
    liftIO $ putMVar lock_client ()


spec :: Spec
spec = do
  describe "Cloud Haskell util test" $
    it "should receive the same number as sent." $
      withTransport $ \transport -> do
        node <- newLocalNode transport initRemoteTable
        let x = 3 :: Int

        yref <- newEmptyMVar
        runProcess node $ do
          lock <- liftIO $ newEmptyMVar
          sp <- spawnChannelLocal $ \rp -> do
                  y <- receiveChan rp
                  liftIO $ putMVar yref y
                  liftIO $ putMVar lock ()
          sendChan sp x
          liftIO $ takeMVar lock
        y <- takeMVar yref
        x `shouldBe` y

  describe "Server/Client communication initialization" $
    it "should pass handshakeTest" $
      withTransport $ \transport -> do
        node <- newLocalNode transport initRemoteTable
        lock <- newLogLock 0
        (lock_server,lock_client) <- (,) <$> newEmptyMVar <*> newEmptyMVar
        forkIO $ runProcess node $
          void $
            flip runReaderT lock $
              runExceptT $
                testHandshake (lock_server,lock_client)
        -- wait until main processes have been completed.
        (,) <$> takeMVar lock_server <*> takeMVar lock_client
        pure ()
      `shouldReturn` ()
