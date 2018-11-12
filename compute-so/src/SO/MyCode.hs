{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SO.MyCode
  ( myApp
  , workerMain
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import           Control.Concurrent.STM   ( TMVar, atomically, putTMVar )
import           Control.Distributed.Process ( ProcessId )
import           Control.Distributed.Process.Lifted ( Process
                                                    , expect, getSelfPid, liftIO, send
                                                    )
import           Control.Distributed.Process.Node ( newLocalNode,runProcess )
import           Control.Exception        ( bracket )
import           Control.Monad            ( forever, void )
import           Control.Monad.Trans.Except ( runExceptT )
import           Control.Monad.Trans.Reader ( runReaderT )
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
import qualified Data.Text             as T
import           Network.HTTP.Types       ( status200 )
import           Network.Transport        ( Transport, closeTransport )
import           Network.Wai              ( Application, responseBuilder )
import           System.IO                ( hPutStrLn, stderr )
------
import           CloudHaskell.Client      ( heartBeatHandshake )
import           CloudHaskell.Server      ( withHeartBeat )
import           CloudHaskell.Util        ( expectSafe
                                          , handleErrorLog
                                          , newLogLock
                                          , onesecond
                                          , tellLog
                                          , tryCreateTransport
                                          )
import           CloudHaskell.Type        ( Pipeline )
import           Network.Transport.UpHere ( DualHostPortPair(..) )
import           Compute.Task             ( rtable )
------
import           Worker.Type              ( WorkerRole(..)
                                          , CellConfig(..)
                                          , NetworkConfig(..)
                                          )


myApp :: MVar Int -> Application
myApp countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 12345
        msg =    fromByteString (B.pack (show count'))
--              <> fromByteString (B.pack (show role))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)


master :: TMVar ProcessId -> Pipeline ()
master ref = do
  self <- getSelfPid
  liftIO $ atomically $ putTMVar ref self

  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (\_ -> pure ()) $ \them_main -> do
    () <- expect  -- for idling
    pure ()


slave :: TMVar ProcessId -> ProcessId -> Pipeline ()
slave ref mpid = do
  heartBeatHandshake mpid $ do
    () <- expect
    pure ()


withTransport :: DualHostPortPair -> (Transport -> IO a) -> IO a
withTransport dhpp action =
  bracket
    (tryCreateTransport dhpp)
    closeTransport
    action


workerMain :: TMVar ProcessId -> (WorkerRole,CellConfig) -> IO ()
workerMain ref (Master name,cellcfg) = do
  let netcfg = cellAddress cellcfg
      dhpp = DHPP
               (T.unpack (hostg netcfg), show (port netcfg))
               (T.unpack (hostg netcfg), show (port netcfg))
  withTransport dhpp $ \transport -> do
     node <- newLocalNode transport rtable
     lock <- newLogLock 0
     runProcess node $
       flip runReaderT lock $
         handleErrorLog $
           master ref
workerMain ref (Slave name mcellcfg mpid,scellcfg) = do
  let snetcfg = cellAddress scellcfg
      mnetcfg = cellAddress mcellcfg
      dhpp = DHPP
               (T.unpack (hostg snetcfg), show (port snetcfg))
               (T.unpack (hostg snetcfg), show (port snetcfg))
  withTransport dhpp $ \transport -> do
    node <- newLocalNode transport rtable
    lock <- newLogLock 0
    forever $ do
      runProcess node $
        flip runReaderT lock $
          handleErrorLog $
            slave ref mpid

      threadDelay (5*onesecond)
