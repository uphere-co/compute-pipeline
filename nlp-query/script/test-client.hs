{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever,void)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import qualified Data.Binary                  as Bi
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text                    as T
import qualified Network.Simple.TCP           as NS
import Network.Transport.TCP (createTransport, defaultTCPParameters)

main :: IO ()
main = do
  let port = ("11115" :: String)
  Right transport <- createTransport ("127.0.0.2") ("11110") defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  mreceived <- connectBroadcast port
  case mreceived of
    Nothing -> error "error"
    Just received -> do
      print received
      void $ runProcess node $ send received (T.pack "Hello")
      
recvAndUnpack :: Bi.Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (Bi.decode . BL.fromStrict) sizebstr :: Bi.Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . Bi.decode . BL.fromStrict) msg

connectBroadcast port = do
  NS.connect "127.0.0.1" port $ \(sock,addr) -> do
    (received :: Maybe ProcessId) <- recvAndUnpack sock
    return received
