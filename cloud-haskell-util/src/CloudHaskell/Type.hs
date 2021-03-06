{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module CloudHaskell.Type where

import           Control.Concurrent.STM.TMVar ( TMVar )
import           Control.DeepSeq              ( NFData )
import           Control.Distributed.Process  ( Process, ProcessId )
import           Control.Monad.Trans.Except   ( ExceptT )
import           Control.Monad.Trans.Reader   ( ReaderT )
import           Data.Binary                  ( Binary(..) )
import           Data.Hashable                ( Hashable )
import           Data.HashMap.Strict          ( HashMap )
import qualified Data.HashMap.Strict as HM
import           Data.Text                    ( Text )
import qualified Data.Text as T
import           Data.Typeable                ( Typeable )
import           GHC.Generics                 ( Generic )

--------------------
-- Error Handling --
--------------------
-- | For error handling
class RenderError e where
  renderError :: e -> Text

-- TODO: We should refrain from using String error.
instance RenderError String where
  renderError = T.pack


data PipelineError = PipelineError String
                   | RouteError String
                   deriving (Show)

instance RenderError PipelineError where
  renderError (PipelineError str) = "pipeline error: " <> T.pack str
  renderError (RouteError str)    = "route error: "    <> T.pack str

type LogLock = (TMVar (),Int)

type Pipeline = ExceptT PipelineError (ReaderT LogLock Process)

data HeartBeat = HB { heartBeat :: Int }

instance Binary HeartBeat where
  put (HB n) = put n
  get = HB <$> get

data Q = Q deriving (Show,Generic)

instance Binary Q
instance NFData Q

data R = R deriving (Show,Generic)

instance Binary R
instance NFData R

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap k v) where
  put = put . HM.toList
  get = HM.fromList <$> get

newtype Router = Router { unRouter :: HashMap Text ProcessId }
               deriving (Show,Binary,Typeable)


newtype TCPPort = TCPPort { unTCPPort :: Int }
                deriving (Show,Eq,Ord,Num)


data Gateway = Gateway { gatewayWeb :: ProcessId
                       , gatewayMaster :: ProcessId
                       }
             deriving (Show,Eq,Generic)



instance Binary Gateway
instance NFData Gateway


data MasterConfig =
  MasterConfig
  { masterBroadcastPort :: TCPPort
  , masterGlobalIP      :: Text
  , masterLocalIP       :: Text
  }
  deriving (Generic,Show)

data SlaveConfig =
  SlaveConfig
  { slavePort         :: TCPPort
  , slaveGlobalIP     :: Text
  , slaveLocalIP      :: Text
  }
  deriving (Generic,Show)


