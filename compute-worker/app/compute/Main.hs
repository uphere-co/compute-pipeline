{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
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
import           Control.Exception   ( throwIO, ErrorCall(..) )
import           Control.Monad       ( forever, void, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import           Data.List           ( find )
import           GHC.Hotswap         ( UpdatableSO
                                     , registerHotswap, swapSO, withSO )
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
import           System.Environment  ( getArgs )
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
-----------------
import           CloudHaskell.Type   ( TCPPort(..)
                                     , Gateway(gatewayMaster)
                                     , MasterConfig(..)
                                     , SlaveConfig(..)
                                     , handleError
                                     )
import           Worker.Type    --     ( SOHandle(..), MasterConfig(..), SlaveConfig(..) )
-----------------
import Compute.Type.Status (Status(..))


pOptions :: Parser ComputeWorkerOption
pOptions = ComputeWorkerOption
           <$> strOption ( long "lang"
                        <> short 'l'
                        <> help "Language engine configuration"
                         )
           <*> strOption ( long "compute"
                        <> short 'c'
                        <> help "Compute pipeline configuration"
                         )

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


main :: IO ()
main = do

  handleError $ do
    cmd <- liftIO $ execParser (info (pCommand <**> helper) (progDesc "compute"))
    case cmd of
      role@(Master opt so_path) -> do
        compcfg :: ComputeConfig <-
          ExceptT $
            eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
        let mConfig = MasterConfig
                      { masterBroadcastPort = TCPPort (port (computeServer compcfg))
                      , masterGlobalIP      = hostg (computeServer compcfg)
                      , masterLocalIP       = hostl (computeServer compcfg)
                      }
            initStatus =
              Status $
                HM.fromList $
                  map (\c -> (cellName c,Nothing)) (computeCells compcfg)
        -- liftIO $ masterMain initStatus mConfig
        liftIO $ do
          putStrLn "start orchestrator"
          -- args <- getArgs
          -- so_path <- case args of
          --   [p] -> return p
          --   _ -> throwIO (ErrorCall "must give file path of first .so as an arg")
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

      Slave cname opt _ -> do
        compcfg :: ComputeConfig
          <- ExceptT $
               eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
        cellcfg <-
          failWith "no such cell" $
            find (\c -> cellName c == cname) (computeCells compcfg)

        let mConfig = MasterConfig
                      { masterBroadcastPort = TCPPort (port (computeServer compcfg))
                      , masterGlobalIP      = hostg (computeServer compcfg)
                      , masterLocalIP       = hostl (computeServer compcfg)
                      }
            sConfig = SlaveConfig
                      { slavePort     = TCPPort (port  (cellAddress cellcfg))
                      , slaveGlobalIP = hostg (cellAddress cellcfg)
                      , slaveLocalIP  = hostl (cellAddress cellcfg)
                      }


        pure ()
