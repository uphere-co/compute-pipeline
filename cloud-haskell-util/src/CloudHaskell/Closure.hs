{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE StaticPointers      #-}
module CloudHaskell.Closure where

import Control.Distributed.Process (Process,NodeId,Closure,SendPort,ReceivePort)
import Control.Distributed.Process.Internal.Closure.BuiltIn (sdictSendPort,staticDecode)
import Control.Distributed.Process.Lifted (spawnChannel)
import Control.Distributed.Process.Lifted.Class (MonadProcess(..))
import Control.Distributed.Process.Serializable  (Serializable
                                                 ,SerializableDict(..))
import Control.Distributed.Static  (Static,closure,closureApply)
import Data.Binary                 (encode)

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

capture' :: (Serializable a) => Static (SerializableDict a) -> a -> Closure a
capture' ssdict = closure (staticDecode ssdict) . encode

capply' :: (Serializable a) => Static (SerializableDict a) -> Closure (a -> b) -> a -> Closure b
capply' ssdict c = closureApply c . capture' ssdict


spawnChannel_ ::
     (MonadProcess m, StaticSerializableDict a) =>
     NodeId
  -> Closure (ReceivePort a -> Process ())
  -> m (SendPort a)
spawnChannel_ = spawnChannel staticSdict
