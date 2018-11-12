{-# LANGUAGE OverloadedStrings #-}
module SO.MyCode
  ( myApp
  , workerMain
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import           Control.Concurrent.STM   ( TMVar, atomically, putTMVar )
import           Control.Distributed.Process ( ProcessId )
import           Control.Distributed.Process.Lifted ( Process, getSelfPid, liftIO, send )
import           Control.Distributed.Process.Node ( newLocalNode,runProcess )
import           Control.Exception        ( bracket )
import           Control.Monad            ( void )
import           Control.Monad.Trans.Except ( runExceptT )
import           Control.Monad.Trans.Reader ( runReaderT )
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
import qualified Data.Text             as T
import           Network.HTTP.Types       ( status200 )
import           Network.Transport        ( closeTransport )
import           Network.Wai              ( Application, responseBuilder )
import           System.IO                ( hPutStrLn, stderr )
------
import           CloudHaskell.Util        ( expectSafe, newLogLock
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

{-
workerMain :: IO ()
workerMain =
  slaveMain (mConfig,sConfig)
    -- TODO: this is not a correct implementation. we should change it. (Why?)
    (\gw -> do
       heartBeatHandshake (gatewayMaster gw) $
         routerHandshake $ \router -> do
           themaster <- lookupRouter "master" router
           send themaster cname
           () <- expect -- this is a kill signal.
           pure ()
    )
-}

master :: TMVar ProcessId -> Pipeline ()
master ref = do
  liftIO $ hPutStrLn stderr "master test"
  self <- getSelfPid
  liftIO $ atomically $ putTMVar ref self
  remotemsg <- expectSafe :: Pipeline Text
  liftIO $ hPutStrLn stderr (show remotemsg)



slave :: TMVar ProcessId -> ProcessId -> Pipeline ()
slave ref mpid = do
  liftIO $ hPutStrLn stderr "slave test"
  send mpid ("Test slave" :: Text)
  pure ()


workerMain :: TMVar ProcessId -> (WorkerRole,CellConfig) -> IO ()
workerMain ref (Master name,cellcfg) = do
  let netcfg = cellAddress cellcfg
      dhpp = DHPP
               (T.unpack (hostg netcfg), show (port netcfg))
               (T.unpack (hostg netcfg), show (port netcfg))
  bracket
    (tryCreateTransport dhpp)
    closeTransport
    (\transport -> do
       hPutStrLn stderr "transport is created"
       node <- newLocalNode transport rtable
       lock <- newLogLock 0
       runProcess node $ void $ flip runReaderT lock $ runExceptT $ master ref
    )
workerMain ref (Slave name mcellcfg mpid,scellcfg) = do
  let snetcfg = cellAddress scellcfg
      mnetcfg = cellAddress mcellcfg
      dhpp = DHPP
               (T.unpack (hostg snetcfg), show (port snetcfg))
               (T.unpack (hostg snetcfg), show (port snetcfg))
  bracket
    (tryCreateTransport dhpp)
    closeTransport
    (\transport -> do
       hPutStrLn stderr "transport is created"
       node <- newLocalNode transport rtable
       lock <- newLogLock 0
       runProcess node $ void $ flip runReaderT lock $ runExceptT $ slave ref mpid

    )
