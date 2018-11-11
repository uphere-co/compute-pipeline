{-# LANGUAGE OverloadedStrings #-}
module SO.MyCode
  ( myApp
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent       ( threadDelay )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import           Control.Exception        ( bracket )
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup           ( (<>) )
import qualified Data.Text             as T
import           Network.HTTP.Types       ( status200 )
import           Network.Transport        ( closeTransport )
import           Network.Wai              ( Application, responseBuilder )
import           System.IO                ( hPutStrLn, stderr )
------
import           CloudHaskell.Util        ( tryCreateTransport )
import           Network.Transport.UpHere ( DualHostPortPair(..) )

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
       threadDelay 10000000
    )
workerMain (Slave _,cellcfg) = do
  hPutStrLn stderr "nothing to do"
