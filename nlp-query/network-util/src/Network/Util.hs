{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Util where

import           Control.Concurrent.STM
import           Control.Distributed.Process (ProcessId)
import qualified Data.Binary                 as Bi
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Network.Simple.TCP          as NS

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

