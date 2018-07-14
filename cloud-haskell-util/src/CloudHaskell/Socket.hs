module CloudHaskell.Socket where

import           Data.Binary                       (Binary,Word32,decode,encode)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Network.Simple.TCP          as NS

recvAndUnpack :: Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (decode . BL.fromStrict) sizebstr :: Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . decode . BL.fromStrict) msg

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Word32
  in BL.toStrict (encode len)

packAndSend :: (Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . encode) x
      sizebstr = packNumBytes msg
  NS.send sock sizebstr
  NS.send sock msg
