{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}
module Worker.API
  ( type SOAPI
  , soAPI
  ) where

import           Data.Proxy               ( Proxy(..) )
import           Data.Text                ( Text )
import           Servant.API              ( (:>), JSON, Post, ReqBody )

type SOAPI = "test" :> ReqBody '[JSON] Text :> Post '[JSON] Text

soAPI :: Proxy SOAPI
soAPI = Proxy
