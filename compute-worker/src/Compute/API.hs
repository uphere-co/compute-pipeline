{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Compute.API where

import           Control.Distributed.Process
                                          ( ProcessId )
import           Data.Binary              ( Binary, decode, encode )
import           Data.Proxy               ( Proxy(Proxy) )
import           Data.Text                ( Text )
import           GHC.Generics             ( Generic )
import           Network.WebSockets       ( WebSocketsData(..), DataMessage(..) )
import           Servant.API              ( Capture, Get, JSON, Post, ReqBody
                                          , (:<|>), (:>)
                                          )
import           Servant.API.WebSocket    ( WebSocket )
------
import           Worker.Type              ( ComputeConfig(..)
                                          , CellConfig(..)
                                          , WorkerRole(..)
                                          )


-- * api

type OrcApiNoStream =
       "compute" :> Get '[JSON] ComputeConfig
  :<|> "cell"    :> Capture "nodeName" Text :> Get '[JSON] (WorkerRole,CellConfig)
  :<|> "process" :> Capture "nodeName" Text :> ReqBody '[JSON] ProcessId :> Post '[JSON] ()
  :<|> "so"      :> Get '[JSON] Text
  :<|> "update"  :> ReqBody '[JSON] Text :> Post '[JSON] ()


type OrcApi =
       "stream"  :> WebSocket
  :<|> OrcApiNoStream


orcApiNoStream :: Proxy OrcApiNoStream
orcApiNoStream = Proxy


orcApi :: Proxy OrcApi
orcApi = Proxy


data SOInfo = SOInfo { soinfoFilePath :: FilePath }
            deriving (Show, Eq, Generic, Binary)

instance WebSocketsData SOInfo where
  fromDataMessage (Text bl _) = decode bl
  fromDataMessage (Binary bl) = decode bl
  fromLazyByteString = decode
  toLazyByteString = encode
