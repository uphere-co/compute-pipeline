{-# LANGUAGE OverloadedStrings   #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )

webApp :: MVar Int -> Application
webApp countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 54321
        msg =    fromByteString (B.pack (show count'))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)

