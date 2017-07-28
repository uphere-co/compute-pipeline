{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Map
import qualified Data.Text               as T
import           Network.Transport
import           Network.Transport.TCP   (createTransport, defaultTCPParameters)
import           Network.Socket.Internal (withSocketsDo)
import           System.Environment

import           Control.Distributed.Process.Node
--
import           OntoNotes.Application.Analyze

-- | Server that echoes messages straight back to the origin endpoint.
echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go empty
  where
    go :: Map ConnectionId (MVar Connection) -> IO ()
    go cs = do
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          putStrLn$ "  New connection: ID "++show cid++", reliability: "++show rel++", address: "++ show addr
          connMVar <- newEmptyMVar
          forkIO $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
          go (insert cid connMVar cs)
        Received cid payload -> do
          forkIO $ do
            node <- newLocalNode transport initRemoteTable
            runProcess node $ runAnalysis (T.intercalate " " $ fmap (T.pack . B.unpack) payload)
            conn <- readMVar (cs ! cid)
            send conn payload
            return ()
          go cs
        ConnectionClosed cid -> do
          putStrLn$ "    Closed connection: ID "++show cid
          forkIO $ do
            conn <- readMVar (cs ! cid)
            close conn
          go (delete cid cs)
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing

main :: IO ()
main = withSocketsDo $ do
  [host, port]    <- getArgs
  serverDone      <- newEmptyMVar
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  forkIO $ echoServer endpoint serverDone
  putStrLn $ "Echo server started at " ++ show (address endpoint)
  readMVar serverDone `onCtrlC` closeTransport transport
