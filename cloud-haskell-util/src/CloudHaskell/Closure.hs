{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE StaticPointers      #-}

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module CloudHaskell.Closure where

import           Control.Distributed.Process (Process,NodeId,Closure,SendPort,ReceivePort)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (sdictSendPort,staticDecode)
import           Control.Distributed.Process.Lifted (spawnChannel)
import           Control.Distributed.Process.Lifted.Class (MonadProcess(..))
import           Control.Distributed.Process.Serializable  (Serializable
                                                           ,SerializableDict(..))
import           Control.Distributed.Static  (Static,closure,closureApply,staticApply,staticPtr)
import           Data.Binary                 (encode)
import           Data.Typeable               (Typeable)
import           GHC.StaticPtr               (StaticPtr)

(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

infixl 9 @@

data Dict c = c => Dict
  deriving Typeable


genReifiedSDict :: Typeable a => Static (Dict (Serializable a) -> SerializableDict a)
genReifiedSDict = staticPtr (static (\Dict -> SerializableDict))


reifiedSDict :: forall a. Typeable a => StaticPtr (Dict (Serializable a)) -> Static (SerializableDict a)
reifiedSDict dict = staticApply genReifiedSDict (staticPtr dict)


capture' :: (Serializable a) => Static (SerializableDict a) -> a -> Closure a
capture' ssdict = closure (staticDecode ssdict) . encode


capply' :: (Serializable a) => Static (SerializableDict a) -> Closure (a -> b) -> a -> Closure b
capply' ssdict c = closureApply c . capture' ssdict


apply ::
  forall a b.
     (Serializable a)
  => StaticPtr (Dict (Serializable a))
  -> Closure (a -> b)
  -> a
  -> Closure b
apply dict c = closureApply c . capture' (reifiedSDict dict)


spawnChannel_ ::
  forall a m.
     (Serializable a, MonadProcess m)
  => StaticPtr (Dict (Serializable a))
  -> NodeId
  -> Closure (ReceivePort a -> Process ())
  -> m (SendPort a)
spawnChannel_ dict = spawnChannel (reifiedSDict dict)
