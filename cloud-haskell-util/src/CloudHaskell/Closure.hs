module CloudHaskell.Closure where

import           Control.Distributed.Process (Closure)
import           Control.Distributed.Static  (closureApply)

class Capture a where
  capture :: a -> Closure a


(@@) :: Closure (a -> b) -> Closure a -> Closure b
(@@) = closureApply

infixl 9 @@

(@<) :: (Capture a) => Closure (a -> b) -> a -> Closure b
(@<) c = closureApply c . capture

infixl 9 @<
