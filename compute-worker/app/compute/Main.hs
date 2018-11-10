{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeSynonymInstances     #-}
-- compute worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- Orchestrator will assign which mode a given compute worker should play a role of.
--
-- In Master mode, the worker has its own external communication channel via REST web
-- API. Through the API, a client will order a command invoking computation and master
-- is supervising slaves for the task.
module Main where

import           Control.Concurrent  ( ThreadId, forkIO, killThread )
import           Control.Concurrent.STM
                                     ( TVar, atomically
                                     , newTVarIO, readTVar, writeTVar
                                     , retry
                                     )
import           Control.Monad       ( forever )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Loops ( iterateM_ )
import           Control.Monad.Trans.Except ( ExceptT(..), withExceptT )
import           Data.Foldable       ( for_ )
import           Data.Text           ( Text )
import qualified Data.Text as T
import           GHC.Hotswap         ( UpdatableSO, registerHotswap, swapSO, withSO )
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp ( run )
import qualified Network.WebSockets.Client as WS ( receiveData, withConnection )
import           Options.Applicative ( Parser
                                     , (<**>)
                                     , execParser
                                     , help
                                     , helper
                                     , info
                                     , long
                                     , progDesc
                                     , short
                                     , strOption
                                     )
import           Servant.API         ((:<|>)((:<|>)))
import           Servant.Client      ( BaseUrl(..), ClientEnv(..), ClientM
                                     , client, parseBaseUrl, runClientM
                                     )
import           System.FilePath     ( (</>) )
import           System.IO           ( hPutStrLn, stderr )
------
import           CloudHaskell.Type   ( handleError )
import           Worker.Type         ( CellConfig
                                     , ComputeConfig(..)
                                     , SOHandle(..)
                                     )
------
import           Compute.Type        ( SOInfo(..), orcApiNoStream )

data WorkerConfig = WorkerConfig { workerConfigOrcURL :: Text
                                 , workerConfigName :: Text
                                 }
                  deriving Show

pOptions :: Parser WorkerConfig
pOptions = WorkerConfig
           <$> strOption ( long "orc"
                        <> short 'o'
                        <> help "URL for orchestrator"
                         )
           <*> strOption ( long "name"
                        <> short 'n'
                        <> help "Name of this node"
                         )


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
getCell :: Text -> ClientM CellConfig
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


main :: IO ()
main = do
  handleError @String $ do
    cfg <- liftIO $ execParser (info (pOptions <**> helper) (progDesc "worker"))
    let url = workerConfigOrcURL cfg
    baseurl <- liftIO $ parseBaseUrl (T.unpack url)
    let wsurl = mkWSURL baseurl
    liftIO $ print url
    liftIO $ print wsurl
    manager <- liftIO $ newManager defaultManagerSettings
    let env = ClientEnv manager baseurl Nothing
    cellcfg <-
      withExceptT show $ ExceptT $
        runClientM (getCell (workerConfigName cfg)) env
    so_path <-
      fmap T.unpack $
        withExceptT show $ ExceptT $
          runClientM (getSO) env
    liftIO $ print (cellcfg,so_path)
    liftIO $ do
      so <- registerHotswap "hs_soHandle" so_path

      ref <- newTVarIO (SOInfo so_path)

      forkIO $
        WS.withConnection wsurl $ \conn ->
          forever $ do
            soinfo :: SOInfo <- WS.receiveData conn
            atomically $ writeTVar ref soinfo

      iterateM_ (looper ref so) Nothing
