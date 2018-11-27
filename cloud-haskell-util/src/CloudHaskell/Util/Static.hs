module CloudHaskell.Util.Static where

import           Control.Distributed.Process.Lifted
                                            ( RemoteTable )
import           Control.Distributed.Static ( registerStatic, staticPtr )
import           Data.Rank1Dynamic          ( toDynamic )
import           Data.Typeable              ( Typeable )
import           GHC.StaticPtr              ( StaticPtr )


-- | register a static pointer to remote table
registerStatic_ ::
     (Typeable a)
  => String
  -> StaticPtr a
  -> RemoteTable
  -> RemoteTable
registerStatic_ name ptr =
  registerStatic name (toDynamic (staticPtr ptr))
