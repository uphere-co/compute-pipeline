module Test.MyTestSpec where

import Control.Concurrent (threadDelay,newEmptyMVar,takeMVar,putMVar)
import Control.Distributed.Process (spawnChannelLocal,sendChan,receiveChan)
import Control.Distributed.Process.Node (initRemoteTable,newLocalNode,runProcess)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Network.Transport (closeTransport)
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import qualified System.IO as IO
import Test.Hspec
------
import MyTest


withTransport action = do
  let host = "127.0.0.1"
  etrnsprt <- createTransport host "12345" (\port' -> (host,port')) defaultTCPParameters
  case etrnsprt of
    Left e ->  fail (show e)
    Right transport -> action transport >>= \r -> closeTransport transport >> pure r

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
                    -- liftIO $ IO.hPrint IO.stderr y
                    liftIO $ putMVar yref y
                    liftIO $ putMVar lock ()
            sendChan sp x
            liftIO $ takeMVar lock

        y <- takeMVar yref
        x `shouldBe` y
