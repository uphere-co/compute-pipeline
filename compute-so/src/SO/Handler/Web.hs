{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import qualified Data.ByteString.Char8 as B
import           Data.Proxy               ( Proxy(..) )
import           Data.Text
import qualified Data.Text as T
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )
import           Servant.API              ( (:>), Get, JSON )


type SOAPI = "test" :> Get '[JSON] Text

soAPI :: Proxy SOAPI
soAPI = Proxy

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

