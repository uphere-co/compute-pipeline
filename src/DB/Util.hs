module DB.Util where

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8

bstrHashToB16 :: ByteString -> String
bstrHashToB16 = BL8.unpack . BL8.fromStrict . B16.encode

b16ToBstrHash :: String -> ByteString
b16ToBstrHash = fst . B16.decode . BL8.toStrict . BL8.pack
