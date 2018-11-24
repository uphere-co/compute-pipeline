{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -w #-}
--
-- `compute worker` is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- Orchestrator will assign which mode a given compute worker should play a role of.
--
-- In Master mode, the worker has its own external communication channel via REST web
-- API. Through the API, a client will order a command invoking computation and master
-- is supervising slaves for the task.
module Compute.Worker where

import           Control.Concurrent  ( ThreadId
                                     , forkFinally, forkIO, killThread
                                     , newEmptyMVar, readMVar
                                     , threadDelay
                                     )
import           Control.Concurrent.STM
                                     ( TVar, atomically
                                     , newEmptyTMVarIO, takeTMVar
                                     , newTVarIO, readTVar, readTVarIO, writeTVar
                                     , retry
                                     )
import           Control.Distributed.Process
                                     ( ProcessId )
import           Control.Monad       ( forever, void )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Loops ( iterateM_ )
import           Control.Monad.Trans.Except ( ExceptT(..), withExceptT )
import           Data.Foldable       ( for_, traverse_ )
import           Data.Text           ( Text )
import qualified Data.Text as T
import           GHC.Hotswap         ( UpdatableSO, registerHotswap, swapSO, withSO )
import           Network.HTTP.Client ( defaultManagerSettings, newManager )
import           Network.Wai.Handler.Warp ( run )
import qualified Network.WebSockets.Client as WS ( receiveData, withConnection )
import           Servant.API         ((:<|>)((:<|>)))
import           Servant.Client      ( BaseUrl(..), ClientM, ClientEnv(..)
                                     , client, parseBaseUrl, runClientM
                                     )
import           System.FilePath     ( (</>) )
import           System.IO           ( hPutStrLn, stderr )
------
import           CloudHaskell.Util   ( onKill, waitForever )
import           Worker.Type         ( CellConfig
                                     , ComputeConfig(..)
                                     , SOHandle(..)
                                     , WorkerRole(..)
                                     )
------
import           Compute.API         ( SOInfo(..), orcApiNoStream )


newtype URL = URL { unURL :: Text }

newtype NodeName = NodeName { unNodeName :: Text }


killSpawned :: TVar [ThreadId] -> IO ()
killSpawned ref = do
  print "parent thread killed."
  tids <- readTVarIO ref
  traverse_ killThread tids

-- NOTE: We collect all of thread ids for child threads created from this root thread.
-- TODO: Use more systematic management. Consider using thread-hierarchy or async-supervisor library.
app :: ClientEnv -> (WorkerRole,CellConfig) ->  UpdatableSO SOHandle -> IO ThreadId
app env (role,cellcfg) sohandle = do
  ref_tids <- newTVarIO []
  flip forkFinally (onKill (killSpawned ref_tids)) $
    withSO sohandle $ \SOHandle{..} -> do
      ref <- newEmptyTMVarIO
      tids <-
        case role of
          Master name -> do
            -- REST API
            tid1 <- forkIO $ run 3994 $ soApplication

            -- update orchestrator with new master process id
            tid2 <- forkIO $ do
              pid <- atomically $ takeTMVar ref
              void $ runClientM (postProcess name pid) env
              print pid
            pure [tid1,tid2]
          Slave  name mpid -> do
            hPutStrLn stderr $ "master pid = " ++ show mpid
            pure []
      tid3 <- soProcess ref (role,cellcfg)
      atomically $ writeTVar ref_tids (tid3 : tids)
      waitForever


looper ::
     ClientEnv
  -> (WorkerRole,CellConfig)
  -> TVar SOInfo
  -> UpdatableSO SOHandle
  -> Maybe (SOInfo,ThreadId)
  -> IO (Maybe (SOInfo,ThreadId))
looper env (role,cellcfg) ref sohandle mcurr  = do
  newso <-
    atomically $ do
      newso <- readTVar ref
      case mcurr of
        Nothing -> pure newso
        Just (currso,_) ->
          if currso == newso then retry else pure newso
  for_ mcurr $ \(_,tid) -> do
    hPutStrLn stderr ("update to " ++ show newso)
    killThread tid
    threadDelay 1000000
    swapSO sohandle (soinfoFilePath newso)
  tid' <- app env (role,cellcfg) sohandle
  pure (Just (newso,tid'))


getCompute :: ClientM ComputeConfig
getCell :: Text -> ClientM (WorkerRole,CellConfig)
postProcess :: Text -> ProcessId -> ClientM ()
getSO :: ClientM Text
postUpdate :: Text -> ClientM ()
getCompute :<|> getCell :<|> postProcess :<|> getSO :<|> postUpdate = client orcApiNoStream


mkWSURL :: BaseUrl -> String
mkWSURL baseurl =
     "ws://"
  ++ baseUrlHost baseurl
  ++ ":"
  ++ show (baseUrlPort baseurl)
  </> baseUrlPath baseurl
  </> "stream"


-- | Actual worker implementation loading process.
--   This loads shared object file and starts looping shared object update routine.
loadWorkerSO :: ClientEnv -> (WorkerRole,CellConfig) -> BaseUrl -> FilePath -> IO ()
loadWorkerSO env (role,cellcfg) baseurl so_path = do
  let wsurl = mkWSURL baseurl
  -- print wsurl
  -- print (cellcfg,so_path)
  so <- registerHotswap "hs_soHandle" so_path
  ref <- newTVarIO (SOInfo so_path)
  forkIO $
    WS.withConnection wsurl $ \conn ->
      forever $ do
        soinfo :: SOInfo <- WS.receiveData conn
        atomically $ writeTVar ref soinfo
  iterateM_ (looper env (role,cellcfg) ref so) Nothing


-- | `runWorker` is the main function for a worker executable.
--   With orchestrator URL and worker's name, it obtains the network information
--   and its role from orchestrator and start actual worker routine.
runWorker :: URL -> NodeName -> ExceptT String IO ()
runWorker (URL url) (NodeName name) = do
  manager' <- liftIO $ newManager defaultManagerSettings
  baseurl <- liftIO $ parseBaseUrl (T.unpack url)
  let env = ClientEnv manager' baseurl Nothing
  (role,cellcfg) <-
    withExceptT show $ ExceptT $
      runClientM (getCell name) env
  -- for debug
  liftIO $ hPutStrLn stderr (show role)
  so_path <-
    fmap T.unpack $
      withExceptT show $ ExceptT $
        runClientM (getSO) env
  liftIO $ loadWorkerSO env (role,cellcfg) baseurl so_path
