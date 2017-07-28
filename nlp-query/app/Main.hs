{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Exception
import qualified Data.Binary             as Bi
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Map
import           Data.Text                    (Text)
import qualified Data.Text               as T
import qualified Network.Simple.TCP           as NS
import           Network.Transport
import           Network.Transport.TCP   (createTransport, defaultTCPParameters)
import           System.Environment

import           Control.Distributed.Process.Node
--
import           OntoNotes.Application.Analyze

main :: IO ()
main = do
  [host, hostG, port, portG] <- getArgs
  pidref              <- newEmptyTMVarIO
  Right transport     <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node $ do
    pid <- spawnLocal $ (liftIO $ print "Server Start!")
    liftIO $ atomically (putTMVar pidref pid)
    liftIO $ broadcast pidref portG hostG

broadcast :: TMVar ProcessId -> String -> String -> IO ()
broadcast pidref portG hostName = do
  pid <- atomically (takeTMVar pidref)
  NS.serve (NS.Host hostName) portG $ \(sock,addr) -> do
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
