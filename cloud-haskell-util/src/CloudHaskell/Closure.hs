{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module CloudHaskell.Closure where

import Control.Distributed.Process (Process,NodeId,Closure,SendPort,ReceivePort)
import Control.Distributed.Process.Internal.Closure.BuiltIn (sdictSendPort,staticDecode)
import Control.Distributed.Process.Lifted (spawnChannel)
import Control.Distributed.Process.Lifted.Class (MonadProcess(..))
import Control.Distributed.Process.Serializable  (Serializable
                                                 ,SerializableDict(..)
                                                 ,TypeableDict(..)
                                                 )
import Control.Distributed.Static  (Static,closure,closureApply,staticApply,staticPtr)
import Data.Binary                 (Binary,encode)
import Data.Typeable               (Typeable)
import GHC.StaticPtr               (StaticPtr,IsStatic)

deriving instance Typeable SerializableDict

data Dict c = c => Dict
  deriving Typeable

class (Serializable a) => StaticSerializableDict a where
  staticSdict :: Static (SerializableDict a)

instance (StaticSerializableDict a) => StaticSerializableDict (SendPort a) where
  staticSdict = sdictSendPort staticSdict

capture :: (StaticSerializableDict a) => a -> Closure a
capture = closure (staticDecode staticSdict) . encode

(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

infixl 9 @@

(@<) :: (StaticSerializableDict a) => Closure (a -> b) -> a -> Closure b
(@<) c = closureApply c . capture

infixl 9 @<

spawnChannel_ ::
     (MonadProcess m, StaticSerializableDict a) =>
     NodeId
  -> Closure (ReceivePort a -> Process ())
  -> m (SendPort a)
spawnChannel_ = spawnChannel staticSdict
