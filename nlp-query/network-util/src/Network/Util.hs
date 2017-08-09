{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Util where

import           Control.Concurrent.STM
import           Control.Distributed.Process (ProcessId)
import qualified Network.Simple.TCP           as NS
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Binary             as Bi

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
