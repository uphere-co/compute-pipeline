{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -w #-}
--
-- Entry point of actual cloud haskell process
--
module SO.Handler.Process
  ( -- * Types and Lenses
    StateCloud(..)
  , cloudSlaves
    -- * Main process
  , main

  ) where

import           Control.Concurrent (MVar, putMVar )
import           Control.Concurrent.STM   ( TVar
                                          , atomically, readTVar, retry
                                          )
import           Control.Distributed.Process.Lifted
                                          ( ProcessId )
import           Control.Error.Safe       ( headZ )
import           Control.Lens             ( (^.), at, makeLenses, to )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Default             ( Default(..) )
import           Data.IntMap              ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe               ( maybe )
import           GHC.Generics             ( Generic )
------
import           CloudHaskell.QueryQueue  ( QQVar )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP
                                          , queryCoreNLP
                                          )
import           Worker.Type              ( StatusProc )


-- | State that keeps the current available slaves.
data StateCloud = StateCloud { _cloudSlaves :: [ProcessId] }
                deriving (Show,Eq,Ord,Generic)

makeLenses ''StateCloud

instance Default StateCloud where
  def = StateCloud [] -- IM.empty

-- | Entry point of main CH process.
--   All the tasks are done inside main by sending process to remote workers.
--
main :: -- TVar StatusProc -> QQVar QCoreNLP RCoreNLP -> MVar (IO ()) -> Pipeline ()
     TVar StateCloud
  -> Pipeline ()
main rCloud = do -- rJava rQQ ref_jvm = do
  tellLog "start mainProcess"
  slave <-
    liftIO $ atomically $ do
      cloud <- readTVar rCloud
      maybe retry pure (cloud ^. cloudSlaves . to headZ)
  tellLog ("got a slave: " ++ show slave)
  -- liftIO $ putMVar ref_jvm (queryCoreNLP rJava rQQ)
