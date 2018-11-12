{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -w #-}
--
-- Orchestrator and workers have websocket communictation.
--
-- Orchestrator has two APIs related to shared object update notification:
--
-- * `/update` post API and
-- * `/streaming` websocket API.
--
-- Each compute worker clients are supposed to connect `/streaming` websocket API when
-- they are initialized. They are initialized with the current state of shared object
-- path information retrieved from orchestrator via `/so` API.
--
-- By `/update`, a client (potential deployment script) will post a new path of share
-- object. Then, orchestrator broadcasts the update to all of its clients. Worker has
-- an event loop to be awakened by the push notification (using TVar retry-when-different
-- cycle), and then it reloads SO file.

module Compute.Orchestrator where

import           Control.Concurrent.STM   ( TVar
                                          , modifyTVar'
                                          , newTVarIO
                                          , readTVar, readTVarIO
                                          , writeTVar
                                          , atomically  )
import           Control.Concurrent.STM.TChan ( TChan, newBroadcastTChanIO
                                              , dupTChan, readTChan, writeTChan )
import           Control.Distributed.Process ( ProcessId )
import           Control.Error.Util       ( failWith )
import           Control.Lens             ( (&), (^.), (.~), (%~)
                                          , makeLenses, view
                                          , _2
                                          , _Just
                                          )
import           Control.Monad            ( forever, when )
import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except ( runExceptT, throwE )
import           Data.List                ( find )
import           Data.Text                ( Text )
import qualified Data.Text as T
import           Data.Traversable         ( for )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setBeforeMainLoop, setPort )
import           Network.WebSockets       ( Connection, forkPingThread, sendBinaryData )
import           Servant                  ( Handler, Server, (:<|>)((:<|>))
                                          , err404, throwError
                                          , serve
                                          )
import           System.IO                ( hPutStrLn, stderr )
------
import           Worker.Type              ( ComputeConfig(..)
                                          , CellConfig(..)
                                          , WorkerRole(..)
                                          )
------
import           Compute.API              ( OrcApi, SOInfo(..), orcApi )


data OrchestratorError = OENoSuchCell Text
                       | OERegisterCellTwice Text
                       | OEMasterNotReady
                       deriving Show


data OrcState = OrcState  {
    _orcStateComputeConfig :: ComputeConfig
  , _orcStateMasterWorker :: Maybe (Text, Maybe ProcessId)
  , _orcStateSlaveWorkers :: [Text]
  }
  deriving (Show)

makeLenses ''OrcState

-- * app

runApp :: (ComputeConfig,FilePath) -> IO ()
runApp (cfg,sofile) = do
  let port = 3123
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      soinfo = SOInfo sofile
  sref <- newTVarIO (OrcState cfg Nothing [])
  ref <- newTVarIO soinfo
  chan <- newBroadcastTChanIO
  runSettings settings $ serve orcApi (server sref ref chan)


server :: TVar OrcState  -- ^ state
       -> TVar SOInfo    -- ^ shared object
       -> TChan SOInfo   -- ^ write-only broadcast channel
       -> Server OrcApi
server sref ref chan =
       wsStream chan
  :<|> getCompute sref
  :<|> getCell sref
  :<|> postProcess sref
  :<|> getSO ref
  :<|> postUpdate ref chan


getCompute :: TVar OrcState -> Handler ComputeConfig
getCompute sref = do
  state <- liftIO $ readTVarIO sref
  pure (state^.orcStateComputeConfig)


-- | getCell API is to determine the role and network configuration.
--   Orchestrator has a state that reflects worker role assignments.
--   When getCell is called, it first checks if the caller id is present in the
--   configuration list, and if not, it throws an error.
--   If no master has been assigned, then it assigns the master role  to the caller.
--   Otherwise, it assigns slave to the caller. Orchestrator updates its state after
--   this change. To be safe, this state update is done atomically in STM monad.
getCell :: TVar OrcState -> Text -> Handler (WorkerRole,CellConfig)
getCell sref name = do
  er <- liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar sref
    let cfg = state ^. orcStateComputeConfig
    c <- failWith (OENoSuchCell name) $
           find (\c -> cellName c == name) (computeCells cfg)
    -- TODO: check if it's already in SlaveWorkers as well.
    case state ^. orcStateMasterWorker of
      Nothing -> do
        lift $ writeTVar sref (state & orcStateMasterWorker .~ Just (name, Nothing))
        pure (Master name, c)
      Just (master,mmpid) -> do
        if (name == master)
          then do
            -- throwE (OERegisterCellTwice name)
            -- NOTE: allow double registration for now.
            -- TODO: revert this when health check is on.
            lift $ writeTVar sref (state & orcStateMasterWorker .~ Just (name, Nothing))
            pure (Master name, c)
          else
            case mmpid of
              Nothing -> throwE OEMasterNotReady
              Just mpid -> do
                lift $ writeTVar sref (state & orcStateSlaveWorkers %~ (name :))
                pure (Slave name mpid,c)

  case er of
    Left  e -> do
      -- for debug
      liftIO (hPutStrLn stderr (show e))
      throwError err404
    Right r -> do
      -- for debug
      s <- liftIO $ readTVarIO sref
      liftIO (hPutStrLn stderr (show s))
      pure r


postProcess :: TVar OrcState -> Text -> ProcessId -> Handler ()
postProcess sref name pid =
  liftIO $ atomically $
    modifyTVar' sref $ (orcStateMasterWorker . _Just . _2) .~ Just pid


getSO :: TVar SOInfo -> Handler Text
getSO ref = liftIO $ do
  SOInfo fp <- readTVarIO ref
  pure (T.pack fp)


postUpdate :: TVar SOInfo -> TChan SOInfo -> Text -> Handler ()
postUpdate ref chan txt = liftIO $ do
  let fp = T.unpack txt
  SOInfo fp' <- readTVarIO ref
  when (fp /= fp') $ do
    print fp
    atomically $ do
      writeTVar ref (SOInfo fp)
      writeTChan chan (SOInfo fp)


wsStream :: TChan SOInfo -> Connection -> Handler ()
wsStream chan c = liftIO $ do
  chan' <- atomically (dupTChan chan)
  forkPingThread c 10
  forever $ do
    soinfo <- atomically $ readTChan chan'
    sendBinaryData c soinfo
