{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Distributed.Process
import qualified Control.Distributed.Process as Cloud
import           Control.Distributed.Process.Node
import           Control.Exception
import           Control.Monad           (forever,void)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Binary             as Bi
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Char8   as B8
import           Data.Map
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           Language.Java              as J
import           Network.Transport
import           Network.Transport.TCP   (createTransport, defaultTCPParameters)
import           System.Environment

import           Control.Distributed.Process.Node
--
import           OntoNotes.App.Analyze
--
import           Network.Util

runQueryServer :: IO ()
runQueryServer = do
  [host, hostB, port, portB] <- getArgs
  pidref              <- newEmptyTMVarIO
  Right transport     <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  config <- loadConfig

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B8.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    pp <- loadJVM
    runProcess node $ do
      pid <- spawnLocal $ forever $ do
        (pid :: ProcessId) <- expect
        (query :: Text) <- expect
        liftIO $ print pid
        liftIO $ print query
        result <- liftIO $ fmap (T.intercalate "\n") (getAnalysis query config pp)
        Cloud.send pid result
        
      liftIO $ do
        atomically (putTMVar pidref pid)
        broadcast pidref portB hostB

