{-# LANGUAGE OverloadedStrings #-}
module SO.MyCode
  ( myApp
  ) where

import Blaze.ByteString.Builder (fromByteString)
import Control.Concurrent.MVar (MVar,modifyMVar)
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseBuilder)

myApp :: MVar Int -> Application
myApp countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 1102
        msg = fromByteString $ B.pack (show count')
    r <- respond $ responseBuilder
           status200
           [("Content-Type", "text/plain")]
           msg
    pure (count', r)
