{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}
module Worker.API
  ( type SOAPI
  , soAPI
  ) where

import           Data.Proxy               ( Proxy(..) )
-- import           Data.Text                ( Text )
import           Servant.API              ( (:>), JSON, Post, ReqBody )
------
import           Task.SemanticParser      ( ComputeQuery, ComputeResult )

type SOAPI = "semantic" :> ReqBody '[JSON] ComputeQuery :> Post '[JSON] ComputeResult

soAPI :: Proxy SOAPI
soAPI = Proxy
