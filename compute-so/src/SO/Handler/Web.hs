{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -w #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.STM   ( TVar, atomically, modifyTVar' )
import           Control.Monad.IO.Class   ( liftIO )
import qualified Data.ByteString.Char8 as B
import           Data.Proxy               ( Proxy(..) )
import           Data.Text                ( Text )
import qualified Data.Text as T
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )
import           Servant                  ( Handler, Server, serve )
import           Servant.API              ( (:>), Get, JSON )


type SOAPI = "test" :> Get '[JSON] Text


soAPI :: Proxy SOAPI
soAPI = Proxy


getTest :: TVar Int -> Handler Text
getTest ref_count = do
  liftIO $ atomically $ modifyTVar' ref_count (+1)
  pure "Hello, there"


server :: TVar Int -> Server SOAPI
server ref_count = getTest ref_count


webApp :: TVar Int -> Application
webApp ref_count = serve soAPI (server ref_count)

{-
_ respond = do
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
-}
