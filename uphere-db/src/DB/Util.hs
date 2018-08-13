module DB.Util where

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Base16     as B16
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

bstrHashToB16 :: ByteString -> Text
bstrHashToB16 = TE.decodeUtf8 . B16.encode

b16ToBstrHash :: Text -> ByteString
b16ToBstrHash = fst . B16.decode . TE.encodeUtf8
