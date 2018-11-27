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
------
-- import           Task.CoreNLP             ( RCoreNLP )
import           Task.SemanticParser      ( ComputeResult )

type SOAPI = "semantic" :> ReqBody '[JSON] Text :> Post '[JSON] ComputeResult

soAPI :: Proxy SOAPI
soAPI = Proxy
