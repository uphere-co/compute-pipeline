{-# LANGUAGE OverloadedStrings #-}
module SO.MyCode
  ( myApp
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup           ( (<>) )
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )
------
import           Worker.Type (WorkerRole)

myApp :: MVar Int -> Application
myApp countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 1102
        msg =    fromByteString (B.pack (show count'))
--              <> fromByteString (B.pack (show role))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)
