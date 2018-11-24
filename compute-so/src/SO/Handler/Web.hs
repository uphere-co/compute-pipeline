{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Control.Monad.IO.Class   ( liftIO )
import           Data.Text                ( Text )
import           Network.Wai              ( Application )
import           Servant                  ( Handler, Server, serve )
------
import           CloudHaskell.QueryQueue  ( type QQVar, singleQuery )
import           Worker.API               ( type SOAPI, soAPI )

getTest :: QQVar Text Text -> Text -> Handler Text
getTest qqvar txt =
  liftIO $ singleQuery qqvar txt



server :: QQVar Text Text -> Server SOAPI
server qqvar = getTest qqvar


webApp :: QQVar Text Text -> Application
webApp qqvar = serve soAPI (server qqvar)
