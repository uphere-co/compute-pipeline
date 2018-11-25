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
import           Task.CoreNLP             ( QCoreNLP(..), RCoreNLP )
import           Worker.API               ( type SOAPI, soAPI )

getCoreNLP :: QQVar QCoreNLP RCoreNLP -> Text -> Handler RCoreNLP
getCoreNLP qqvar txt = do
  liftIO $ singleQuery qqvar (QCoreNLP txt)
  -- pure (T.pack (show r))


server :: QQVar QCoreNLP RCoreNLP -> Server SOAPI
server qqvar = getCoreNLP qqvar


webApp :: QQVar QCoreNLP RCoreNLP -> Application
webApp qqvar = serve soAPI (server qqvar)
