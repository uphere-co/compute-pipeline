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
import           Servant.API              ( (:>), JSON, Post, ReqBody )
------
import           CloudHaskell.QueryQueue  ( type QQVar, singleQuery )

type SOAPI = "test" :> ReqBody '[JSON] Text :> Post '[JSON] Text


soAPI :: Proxy SOAPI
soAPI = Proxy


getTest :: QQVar Text Text -> Text -> Handler Text
getTest qqvar txt =
  liftIO $ singleQuery qqvar txt



server :: QQVar Text Text -> Server SOAPI
server qqvar = getTest qqvar


webApp :: QQVar Text Text -> Application
webApp qqvar = serve soAPI (server qqvar)
