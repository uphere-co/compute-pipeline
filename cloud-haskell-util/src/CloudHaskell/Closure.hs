{-# LANGUAGE MonoLocalBinds #-}
module CloudHaskell.Closure where

import           Control.Distributed.Process (Closure,SendPort)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (sdictSendPort,staticDecode)
import           Control.Distributed.Process.Serializable  (Serializable,SerializableDict)
import           Control.Distributed.Static  (Static,closure,closureApply)
import           Data.Binary                 (encode)

class Capture a where
  capture :: a -> Closure a
  staticSdict :: Static (SerializableDict a)

instance (Serializable a, Capture a) => Capture (SendPort a) where
  capture = closure (staticDecode (sdictSendPort staticSdict)) . encode
  staticSdict = sdictSendPort staticSdict

(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

infixl 9 @@

(@<) :: (Capture a) => Closure (a -> b) -> a -> Closure b
(@<) c = closureApply c . capture

infixl 9 @<
