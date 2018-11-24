{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Control.Monad.IO.Class   ( liftIO )
import           Data.Proxy               ( Proxy(..) )
import           Data.Text                ( Text )
import           Network.Wai              ( Application )
import           Servant                  ( Handler, Server, serve )
import           Servant.API              ( (:>), Get, JSON )
------
import           CloudHaskell.QueryQueue  ( type QQVar, singleQuery )

type SOAPI = "test" :> Get '[JSON] Text


soAPI :: Proxy SOAPI
soAPI = Proxy


getTest :: QQVar Text Text -> Handler Text
getTest qqvar = do
  r <- liftIO $ singleQuery qqvar "my test"
  pure r -- "Hello, there"


server :: QQVar Text Text -> Server SOAPI
server qqvar = getTest qqvar


webApp :: QQVar Text Text -> Application
webApp qqvar = serve soAPI (server qqvar)
