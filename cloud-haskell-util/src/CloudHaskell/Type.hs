{-# LANGUAGE DeriveGeneric #-}
module CloudHaskell.Type where

import           Control.Concurrent.STM.TMVar (TMVar)
import           Control.DeepSeq              (NFData)
import           Control.Distributed.Process  (Process)
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Data.Binary                  (Binary(..))
import           GHC.Generics                 (Generic)

data PipelineError = PipelineError String
                   | RouteError String
                   deriving (Show)

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
