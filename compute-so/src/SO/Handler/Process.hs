{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -w #-}
--
-- Module for cloud haskell process entry points.
-- This module provides main and remote table.
--
--
module SO.Handler.Process
  ( -- * Types and Lenses
    StateCloud(..)
  , cloudSlaves
    -- * Main process
  , main
    -- * Remote table
  , rtable
  ) where

import           Control.Concurrent (MVar, putMVar )
import           Control.Concurrent.STM   ( TVar
                                          , atomically, readTVar, retry
                                          )
import           Control.Distributed.Process.Lifted
                                          ( Process
                                          , ProcessId(..)
                                          , ReceivePort
                                          , RemoteTable
                                          , SendPort
                                          , sendChan
                                          , receiveChan
                                          , spawnChannel
                                          )
import           Control.Distributed.Process.Node.Lifted
                                          ( initRemoteTable )
import           Control.Distributed.Process.Serializable ( SerializableDict(..) )
import           Control.Distributed.Static ( registerStatic, staticClosure, staticPtr )
import           Control.Error.Safe       ( headZ )
import           Control.Lens             ( (^.), at, makeLenses, to )
import           Control.Monad            ( forever, void )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Default             ( Default(..) )
import           Data.IntMap              ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe               ( maybe )
import           Data.Rank1Dynamic        ( toDynamic )
import           Data.Text                ( Text )
import           GHC.Generics             ( Generic )
------
-- temp
import           SRL.Analyze.Type         ( DocAnalysisInput(..) )
------
import           CloudHaskell.QueryQueue  ( QQVar, handleQuery )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP(..)
                                          , queryCoreNLP
                                          )
import           Worker.Type              ( StatusProc )


-- | State that keeps the current available slaves.
data StateCloud = StateCloud { _cloudSlaves :: [ProcessId] }
                deriving (Show,Eq,Ord,Generic)

makeLenses ''StateCloud

instance Default StateCloud where
  def = StateCloud []


dummyOutput = RCoreNLP (DocAnalysisInput [] [] [] [] [] [] Nothing)

-- | Entry point of main CH process.
--   All the tasks are done inside main by sending process to remote workers.
--
main :: -- TVar StatusProc  -> MVar (IO ()) -> Pipeline ()
     TVar StateCloud
  -> QQVar QCoreNLP RCoreNLP
  -> Pipeline ()
main rCloud rQQ = do -- rJava rQQ ref_jvm = do
  tellLog "start mainProcess"
  slave <-
    liftIO $ atomically $ do
      cloud <- readTVar rCloud
      maybe retry pure (cloud ^. cloudSlaves . to headZ)
  tellLog ("got a slave: " ++ show slave)
  let slaveNode = processNodeId slave

  sCNLP <-
    spawnChannel
      (staticPtr (static (SerializableDict @QCoreNLP)))
      slaveNode
      (staticClosure (staticPtr (static echo)))

  handleQuery rQQ $ \q -> do
    sendChan sCNLP q
    pure dummyOutput

  -- liftIO $ putMVar ref_jvm (queryCoreNLP rJava rQQ)


-- | Global remote table.
--
--   NOTE: Registering mechanism in CH here is manual with static
--         pointer, with an intent to make the process explicit.
rtable :: RemoteTable
rtable =
  registerStatic
    "$echo"
    (toDynamic (staticPtr (static echo)))
    initRemoteTable

echo :: ReceivePort QCoreNLP -> Process ()
echo rCNLP = do
  liftIO $ putStrLn "echo program is launched."
  forever $ do
    QCoreNLP msg <- receiveChan rCNLP
    liftIO $ putStrLn $ "msg echoed: " ++ show msg
