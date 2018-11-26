{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
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
  , main      -- master
  , mainSlave -- slave
    -- * Remote table
  , rtable
  ) where

import           Control.Concurrent (MVar, newEmptyMVar, putMVar, threadDelay )
import           Control.Concurrent.STM   ( TVar
                                          , atomically
                                          , newTVarIO
                                          , readTVar
                                          , retry
                                          )
import           Control.Distributed.Process.Lifted
                                          ( Process
                                          , ProcessId(..)
                                          , ReceivePort
                                          , RemoteTable
                                          , SendPort
                                          , expect
                                          , newChan
                                          , send
                                          , sendChan
                                          , receiveChan
                                          , spawnChannel
                                          , spawnLocal
                                          , unStatic
                                          )
import           Control.Distributed.Process.Node.Lifted
                                          ( initRemoteTable )
import           Control.Distributed.Process.Serializable ( SerializableDict(..) )
import           Control.Distributed.Static
                                          ( Static
                                          , registerStatic
                                          , staticClosure
                                          , staticPtr
                                          )
import           Control.Error.Safe       ( headZ )
import           Control.Lens             ( (^.), at, makeLenses, to )
import           Control.Monad            ( forever, void )
import           Control.Monad.IO.Class   ( liftIO )
import           Data.Default             ( Default(..) )
import           Data.IntMap              ( IntMap )
import qualified Data.IntMap as IM
import Data.IORef (IORef,newIORef,readIORef,modifyIORef')
import           Data.Maybe               ( maybe )
import           Data.Rank1Dynamic        ( toDynamic )
import           Data.Text                ( Text )
import           GHC.Generics             ( Generic )
import System.IO.Unsafe (unsafePerformIO)
------
-- temp
import           SRL.Analyze.Type         ( DocAnalysisInput(..) )
------
import           CloudHaskell.Closure     ( capply' )
import           CloudHaskell.QueryQueue  ( QQVar
                                          , emptyQQ
                                          , handleQuery
                                          , singleQuery
                                          )
import           CloudHaskell.Util        ( expectSafe, tellLog )
import           CloudHaskell.Type        ( Pipeline )
import           Task.CoreNLP             ( QCoreNLP(..)
                                          , RCoreNLP(..)
                                          , queryCoreNLP
                                          )
import           Worker.Type              ( StatusProc, javaProc, javaProcStatus )



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

  let process =
        capply'
          (staticPtr (static (SerializableDict @(Static (TVar StatusProc)))))
          (capply'
            (staticPtr (static (SerializableDict @(Static (MVar (IO ()))))))
            (staticClosure (staticPtr (static daemonCoreNLP)))
            (staticPtr (static javaProc))
          )
          (staticPtr (static javaProcStatus))

  sQ <-
    spawnChannel
      (staticPtr (static (SerializableDict @(QCoreNLP,SendPort RCoreNLP))))
      slaveNode
      process

  handleQuery rQQ $ \q -> do
    (sR,rR) <- newChan
    tellLog $ show q
    sendChan sQ (q,sR)
    r <- receiveChan rR
    pure r



-- | Global remote table.
--
--   NOTE: Registering mechanism in CH here is manual with static
--         pointer, with an intent to make the process explicit.
rtable :: RemoteTable
rtable =
  registerStatic
    "$daemonCoreNLP"
    (toDynamic (staticPtr (static daemonCoreNLP)))
  $ registerStatic
    "$javaProc"
    (toDynamic (staticPtr (static javaProc)))
  $ registerStatic
    "$javaProcStatus"
    (toDynamic (staticPtr (static javaProcStatus)))
  $ initRemoteTable

daemonCoreNLP ::
     Static (MVar (IO ()))
  -> Static (TVar StatusProc)
  -> ReceivePort (QCoreNLP, SendPort RCoreNLP)
  -> Process ()
daemonCoreNLP pJProc pJProcStatus rQ = do
  liftIO $ putStrLn "echo program is launched."

  rQQ <- liftIO $ newTVarIO emptyQQ
  spawnLocal $ do
    forever $ do
      -- receive query
      (q,sR) <- receiveChan rQ
      -- liftIO $ putStrLn $ "msg echoed: " ++ show msg
      r <- liftIO $ singleQuery rQQ q
      -- send answer
      sendChan sR r

  jproc <- unStatic pJProc
  jprocStatus <- unStatic pJProcStatus
  liftIO $ putMVar jproc (queryCoreNLP jprocStatus rQQ)

  -- wait indefinitely
  () <- expect
  pure ()
{-
  spawnLocal $ do
    liftIO $ putMVar jproc (putStrLn "java process is init") -}
    {- forever $ liftIO $ do
      threadDelay 500000
      modifyIORef' ref (+1)
    -}
  {-
  (sStat,rStat) <- newChan
  send nodeManager sStat

  rTest <- receiveChan rStat
  liftIO $ putMVar rTest ("abc" :: Text)
  -}
  -- e  :: StatusProc <- liftIO $ atomically $ readTVar rJava
  -- liftIO $ print e
  -- sStat' <- receiveChan rStat
  -- sendChan sStat' ("abcdef" :: Text)


mainSlave :: Pipeline ()
mainSlave = do
  tellLog "mainSlave"

{-
  liftIO $ forever $ do
    threadDelay 1000000
    r <- readIORef ref_test
    print r
-}

  {-
  rTest <- liftIO $ newEmptyMVar
  let p = staticPtr (static rTest)
  sStat <- expectSafe

  -- (sStat',rStat') <- newChan
  sendChan sStat p -- rJava -- sStat'
  -}
  -- r :: Text <- receiveChan rStat'
  -- tellLog $ "test: " ++ show r
