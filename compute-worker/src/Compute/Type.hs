{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Compute.Type where

import           Data.Proxy               ( Proxy(Proxy) )
import           Data.Text                ( Text )
import           Servant.API              ( Capture, Get, JSON, Post, ReqBody
                                          , (:<|>), (:>)
                                          )
import           Servant.API.WebSocket    ( WebSocket )
------
import           Worker.Type              ( ComputeConfig(..), CellConfig(..) )


-- * api

type OrcApi = "compute" :> Get '[JSON] ComputeConfig
         :<|> "cell"    :> Capture "nodeName" Text :> Get '[JSON] CellConfig
         :<|> "so"      :> Get '[JSON] Text
         :<|> "update"  :> ReqBody '[JSON] Text :> Post '[JSON] ()
         :<|> "stream"  :> WebSocket

orcApi :: Proxy OrcApi
orcApi = Proxy

