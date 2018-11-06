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
import           Control.Error.Util  ( failWith )
import           Control.Monad       ( forever, void, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Data.List           ( find )
import           Data.Text           ( Text )
import qualified Data.Text as T
import           GHC.Hotswap         ( UpdatableSO
                                     , registerHotswap, swapSO, withSO )
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp ( run )
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
import           Servant.API
import           Servant.Client
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
import           Compute.Type        ( OrcApi, orcApi )

data WorkerConfig = WorkerConfig { workerConfigOrcURL :: Text }
                  deriving Show

pOptions :: Parser WorkerConfig
pOptions = WorkerConfig
           <$> strOption ( long "orc"
                        <> short 'o'
                        <> help "URL for orchestrator"
                         )

{-
pSOFile :: Parser FilePath
pSOFile = strOption ( long "sofile"
                   <> short 's'
                   <> help "SO File"
                    )


pCommand :: Parser WorkerRole
pCommand =
  subparser
     ( command "master"
         (info
           (Master <$> pOptions <*> pSOFile)
           (progDesc "running as master")
         )
    <> command "slave"
         (info
           (Slave  <$> strOption (long "name" <> short 'n' <> help "Cell name")
                   <*> pOptions
                   <*> pSOFile
           )
           (progDesc "running as slave")
         )
     )
-}

looper :: UpdatableSO SOHandle -> WorkerRole -> IO ()
looper so role =
  withSO so $ \SOHandle{..} ->
    run 3994 $ soApplication role


notified :: UpdatableSO SOHandle -> FilePath -> MVar () -> ThreadId -> Event -> IO ()
notified so basepath lock tid e =
 case e of
   Created _ fp_bs -> do
     let fp =  basepath </> B.unpack fp_bs
     when (takeExtension fp == ".o") $ do
       hPutStrLn stderr ("swap SO file: " ++ fp)
       killThread tid
       swapSO so fp
       putMVar lock ()
   _ -> pure ()


getCompute :: ClientM ComputeConfig
getCell :: Text -> ClientM CellConfig

getCompute :<|> getCell = client orcApi

main :: IO ()
main = do

  handleError @String $ do
    cfg <- liftIO $ execParser (info (pOptions <**> helper) (progDesc "worker"))
    let url = workerConfigOrcURL cfg
    baseurl <- liftIO $ parseBaseUrl (T.unpack url)
    liftIO $ print url
    manager <- liftIO $ newManager defaultManagerSettings
    let env = ClientEnv manager baseurl Nothing
    r <- liftIO $ runClientM getCompute env
    liftIO $ print r
    {-
      ExceptT $
        eitherDecodeStrict <$> B.readFile (servComputeConfig opt)

           _compcfg :: ComputeConfig <-
        liftIO $ do
          putStrLn "start orchestrator"
          let so_dir = takeDirectory so_path
              so_dir_bs = B.pack (so_dir)

          so <- registerHotswap "hs_soHandle" so_path

          forever $ do
            tid <- forkIO $ looper so role

            withINotify $ \inotify -> do
              lock <- newEmptyMVar
              addWatch inotify [Create] so_dir_bs (notified so so_dir lock tid)
              -- idling
              void $ takeMVar lock
  -}
