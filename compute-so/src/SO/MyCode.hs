{-# LANGUAGE OverloadedStrings #-}
module SO.MyCode
  ( myApp
  , workerMain
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import           Control.Distributed.Process.Lifted ( Process, liftIO )
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


{-
import           Control.Distributed.Process.Lifted
                                     ( expect, send )
import           Control.Error.Util  ( failWith )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as HM
import           Data.List           ( find )
import           Data.Semigroup      ( (<>) )
import           Data.Text           ( Text )
-----------------
import           CloudHaskell.Client ( heartBeatHandshake, routerHandshake )
import           CloudHaskell.Type   ( TCPPort(..)
                                     , Gateway(gatewayMaster)
                                     , MasterConfig(..)
                                     , SlaveConfig(..)
                                     , handleError
                                     )
import           CloudHaskell.Util   ( lookupRouter )
-----------------
import           Compute             ( masterMain, slaveMain )
import           Compute.Type.Status ( Status(..) )

import Worker.Type
-}

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

testProcess :: Pipeline ()
testProcess = do
  liftIO $ hPutStrLn stderr "testProcess"
  remotemsg <- expectSafe :: Pipeline Text
  liftIO $ hPutStrLn stderr (show remotemsg)


workerMain :: (WorkerRole,CellConfig) -> IO ()
workerMain (Master,cellcfg) = do
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
       runProcess node $ void $ flip runReaderT lock $ runExceptT $ testProcess
    )
workerMain (Slave mcellcfg,scellcfg) = do
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
       runProcess node $ void $ flip runReaderT lock $ runExceptT $ do
         liftIO $ hPutStrLn stderr "in process"

    )
  hPutStrLn stderr "nothing to do"
