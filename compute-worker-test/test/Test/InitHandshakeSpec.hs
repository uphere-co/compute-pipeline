{-# LANGUAGE TypeApplications #-}
module Test.InitHandshakeSpec where

import Control.Concurrent          ( MVar
                                   , threadDelay
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
import Control.Distributed.Process.Node.Lifted (initRemoteTable,newLocalNode,runProcess)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Network.Transport (closeTransport)
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import qualified System.IO as IO
import Test.Hspec
------
import CloudHaskell.Client (heartBeatHandshake)
import CloudHaskell.Server (withHeartBeat)
import CloudHaskell.Type (Pipeline)
import CloudHaskell.Util (expectSafe,newLogLock)



withTransport action = do
  let host = "127.0.0.1"
  etrnsprt <- createTransport host "12345" (\port' -> (host,port')) defaultTCPParameters
  case etrnsprt of
    Left e ->  fail (show e)
    Right transport -> action transport >>= \r -> closeTransport transport >> pure r


testHandshake (lock_server,lock_client) = do
  server_ping <- getSelfPid
  spawnLocal (client lock_client server_ping)
  server lock_server

server :: MVar () -> Pipeline ()
server lock_server = do
  client_ping <- expectSafe @ProcessId
  withHeartBeat client_ping (\_ -> pure ()) $ \client_main ->
    liftIO $ putMVar lock_server ()

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
        y <-
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

  describe "Heartbeat test" $
    it "should ping/pong heartbeat" $
      withTransport $ \transport -> do
        node <- newLocalNode transport initRemoteTable
        lock <- newLogLock 0
        lock_server <- newEmptyMVar
        lock_client <- newEmptyMVar
        runProcess node $
          void $
            flip runReaderT lock $
              runExceptT $
                testHandshake (lock_server,lock_client)
        pure ()
      `shouldReturn` ()
