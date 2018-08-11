{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE StaticPointers      #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module CloudHaskell.Closure where

import Control.Distributed.Process (Process,NodeId,Closure,SendPort,ReceivePort)
import Control.Distributed.Process.Internal.Closure.BuiltIn (sdictSendPort,staticDecode)
import Control.Distributed.Process.Lifted (spawnChannel)
import Control.Distributed.Process.Lifted.Class (MonadProcess(..))
import Control.Distributed.Process.Serializable  (Serializable
                                                 ,SerializableDict(..))
import Control.Distributed.Static  (Static,closure,closureApply)
import Data.Binary                 (encode)


(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

infixl 9 @@

capture' :: (Serializable a) => Static (SerializableDict a) -> a -> Closure a
capture' ssdict = closure (staticDecode ssdict) . encode

capply' :: (Serializable a) => Static (SerializableDict a) -> Closure (a -> b) -> a -> Closure b
capply' ssdict c = closureApply c . capture' ssdict
