{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings  #-}
module SO.Handler.Web
  ( webApp
  ) where

import           Control.Monad.IO.Class   ( liftIO )
import           Network.Wai              ( Application )
import           Servant                  ( Handler, Server, serve )
------
import           CloudHaskell.QueryQueue  ( type QQVar, singleQuery )
import           Task.SemanticParser      ( ComputeQuery(..), ComputeResult(..) )
import           Worker.API               ( type SOAPI, soAPI )


getSemantic :: QQVar ComputeQuery ComputeResult -> ComputeQuery -> Handler ComputeResult
getSemantic rQQ query = do
  liftIO $ singleQuery rQQ query


server :: QQVar ComputeQuery ComputeResult -> Server SOAPI
server rQQ = getSemantic rQQ


webApp :: QQVar ComputeQuery ComputeResult -> Application
webApp rQQ = serve soAPI (server rQQ)
