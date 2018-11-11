-- {-# LANGUAGE FlexibleInstances        #-}
-- {-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
-- {-# LANGUAGE TypeSynonymInstances     #-}
-- compute worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- Orchestrator will assign which mode a given compute worker should play a role of.
--
-- In Master mode, the worker has its own external communication channel via REST web
-- API. Through the API, a client will order a command invoking computation and master
-- is supervising slaves for the task.
module Compute.Worker where

import           Control.Concurrent  ( ThreadId, forkIO, killThread )
import           Control.Concurrent.STM
                                     ( TVar, atomically
                                     , newTVarIO, readTVar, writeTVar
                                     , retry
                                     )
import           Control.Monad       ( forever )
import           Control.Monad.Loops ( iterateM_ )
import           Data.Foldable       ( for_ )
import           Data.Text           ( Text )
import           GHC.Hotswap         ( UpdatableSO, registerHotswap, swapSO, withSO )
import           Network.Wai.Handler.Warp ( run )
import qualified Network.WebSockets.Client as WS ( receiveData, withConnection )
import           Servant.API         ((:<|>)((:<|>)))
import           Servant.Client      ( BaseUrl(..), ClientM, client )
import           System.FilePath     ( (</>) )
import           System.IO           ( hPutStrLn, stderr )
------
import           Worker.Type         ( CellConfig
                                     , ComputeConfig(..)
                                     , SOHandle(..)
                                     , WorkerRole(..)
                                     )
------
import           Compute.Type        ( SOInfo(..), orcApiNoStream )



app :: UpdatableSO SOHandle -> IO ()
app sohandle =
  withSO sohandle $ \SOHandle{..} ->
    run 3994 $ soApplication


looper ::
     TVar SOInfo
  -> UpdatableSO SOHandle
  -> Maybe (SOInfo,ThreadId)
  -> IO (Maybe (SOInfo,ThreadId))
looper ref sohandle mcurr  = do
  newso <-
    atomically $ do
      newso <- readTVar ref
      case mcurr of
        Nothing -> pure newso
        Just (currso,_) ->
          if currso == newso then retry else pure newso
  for_ mcurr $ \(_,tid) -> do
    hPutStrLn stderr ("update to" ++ show newso)
    killThread tid
    swapSO sohandle (soinfoFilePath newso)
  tid' <- forkIO $ app sohandle
  pure (Just (newso,tid'))



getCompute :: ClientM ComputeConfig
getCell :: Text -> ClientM (WorkerRole,CellConfig)
getSO :: ClientM Text
postUpdate :: Text -> ClientM ()
getCompute :<|> getCell :<|> getSO :<|> postUpdate = client orcApiNoStream


mkWSURL :: BaseUrl -> String
mkWSURL baseurl =
     "ws://"
  ++ baseUrlHost baseurl
  ++ ":"
  ++ show (baseUrlPort baseurl)
  </> baseUrlPath baseurl
  </> "stream"

runWorker :: CellConfig -> BaseUrl -> FilePath -> IO ()
runWorker cellcfg baseurl so_path = do
  let wsurl = mkWSURL baseurl
  print wsurl
  print (cellcfg,so_path)
  so <- registerHotswap "hs_soHandle" so_path

  ref <- newTVarIO (SOInfo so_path)

  forkIO $
    WS.withConnection wsurl $ \conn ->
      forever $ do
        soinfo :: SOInfo <- WS.receiveData conn
        atomically $ writeTVar ref soinfo

  iterateM_ (looper ref so) Nothing

