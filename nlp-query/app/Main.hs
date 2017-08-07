{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import qualified Network.Simple.TCP           as NS
import           Network.Transport
import           Network.Transport.TCP   (createTransport, defaultTCPParameters)
import           System.Environment

import           Control.Distributed.Process.Node
--
import           OntoNotes.App.Analyze

main :: IO ()
main = do
  [host, hostB, port, portB] <- getArgs
  pidref              <- newEmptyTMVarIO
  Right transport     <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B8.pack ("-Djava.class.path=" ++ clspath) ] $ do 
    pp <- loadJVM
    runProcess node $ do
      pid <- spawnLocal $ forever $ do
        (pid :: ProcessId) <- expect
        (query :: Text) <- expect
        liftIO $ print pid
        liftIO $ print query
        result <- liftIO $ getAnalysisResult query pp
        Cloud.send pid result
        
      liftIO $ do
        atomically (putTMVar pidref pid)
        broadcast pidref portB hostB
    
broadcast :: TMVar ProcessId -> String -> String -> IO ()
broadcast pidref portB hostName = do
  pid <- atomically (takeTMVar pidref)
  NS.serve (NS.Host hostName) portB $ \(sock,addr) -> do
    print $ "Request from " ++ (show addr)
    packAndSend sock pid

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Bi.Word32
  in BL.toStrict (Bi.encode len)

packAndSend :: (Bi.Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . Bi.encode) x
      sizebstr = packNumBytes msg
  NS.send sock sizebstr
  NS.send sock msg
