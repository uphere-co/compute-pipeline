{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Compute.Type where

import           Data.Proxy               ( Proxy(Proxy) )
import           Data.Text                ( Text )
import           Servant.API              ( Capture, Get, JSON
                                          , (:<|>), (:>)
                                          )
------
import           Worker.Type              ( ComputeConfig(..), CellConfig(..) )


-- * api

type OrcApi = "compute" :> Get '[JSON] ComputeConfig
         :<|> "cell"    :> Capture "nodeName" Text :> Get '[JSON] CellConfig

orcApi :: Proxy OrcApi
orcApi = Proxy

