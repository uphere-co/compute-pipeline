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
import           Task.CoreNLP             ( RCoreNLP )

type SOAPI = "corenlp" :> ReqBody '[JSON] Text :> Post '[JSON] RCoreNLP

soAPI :: Proxy SOAPI
soAPI = Proxy
