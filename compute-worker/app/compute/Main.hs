{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# OPTIONS_GHC -w #-}
-- compute-worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- With configuration, master and named slave will be assigned with
-- IP address and they automatically find each other.
-- Once ready, master will start a required process as ordered by REST API.
module Main where

import           Control.Concurrent  ( MVar, ThreadId
                                     , forkIO, killThread
                                     , newEmptyMVar, putMVar, takeMVar
                                     )
import           Control.Concurrent.STM
                                     ( TVar, atomically
                                     , newTVarIO, readTVar, writeTVar
                                     , retry
                                     )
import           Control.Error.Util  ( failWith, hoistEither )
import           Control.Monad       ( forever, void, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Loops ( iterateM_ )
import           Control.Monad.Trans.Except ( ExceptT(..), withExceptT )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Data.Foldable       ( for_ )
import           Data.List           ( find )
import           Data.Text           ( Text )
import qualified Data.Text as T
import           GHC.Hotswap         ( UpdatableSO
                                     , registerHotswap, swapSO, withSO )
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp ( run )
import qualified Network.WebSockets.Client as WS ( receiveData, withConnection )
import           Options.Applicative ( Parser
                                     , (<**>)
                                     , command
                                     , execParser
                                     , help
                                     , helper
                                     , info
                                     , long
                                     , progDesc
                                     , short
                                     , strOption
                                     , subparser
                                     )
import           Servant.API         ((:<|>)((:<|>)))
import           Servant.Client      ( BaseUrl(..), ClientEnv(..), ClientM
                                     , client, parseBaseUrl, runClientM
                                     )
import           System.FilePath     ( (</>)
                                     , takeDirectory
                                     , takeExtension
                                     )
import           System.INotify      ( Event(..)
                                     , EventVariety(..)
                                     , addWatch
                                     , withINotify
                                     )
import           System.IO           ( hPutStrLn, stderr )
------
import           CloudHaskell.Type   ( handleError )
import           Worker.Type         ( CellConfig(..)
                                     , ComputeConfig(..)
                                     , ComputeWorkerOption(..)
                                     , SOHandle(..)
                                     , WorkerRole(..)
                                     , cellName
                                     )
------
import           Compute.Type        ( OrcApiNoStream, SOInfo(..), orcApiNoStream )

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
        Just (currso,tid) ->
          if currso == newso then retry else pure newso
  for_ mcurr $ \(_,tid) -> do
    putStrLn ("update to" ++ show newso)
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
      let so_dir = takeDirectory so_path
          so_dir_bs = B.pack (so_dir)

      so <- registerHotswap "hs_soHandle" so_path

      ref <- newTVarIO (SOInfo so_path)

      forkIO $
        WS.withConnection wsurl $ \conn ->
          forever $ do
            soinfo :: SOInfo <- WS.receiveData conn
            atomically $ writeTVar ref soinfo

      iterateM_ (looper ref so) Nothing
