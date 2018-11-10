{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Compute.Type where

import           Data.Proxy               ( Proxy(Proxy) )
import           Data.Text                ( Text )
import           Servant.API              ( Capture, Get, JSON
                                          , (:<|>), (:>)
                                          )
import           Servant.API.WebSocket    ( WebSocket )
------
import           Worker.Type              ( ComputeConfig(..), CellConfig(..) )


-- * api

type OrcApi = "compute" :> Get '[JSON] ComputeConfig
         :<|> "cell"    :> Capture "nodeName" Text :> Get '[JSON] CellConfig
         :<|> "so"      :> Get '[JSON] Text
         :<|> "stream"  :> WebSocket

orcApi :: Proxy OrcApi
orcApi = Proxy

