{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import qualified Data.Binary                  as Bi
import qualified Data.ByteString.Lazy         as BL
import qualified Network.Simple.TCP           as NS
import Network.Transport.TCP (createTransport, defaultTCPParameters)

main :: IO ()
main = do
  let port = ("11115" :: String)
  forever $ connectBroadcast port

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
    (received :: Maybe String) <- recvAndUnpack sock
    print received
