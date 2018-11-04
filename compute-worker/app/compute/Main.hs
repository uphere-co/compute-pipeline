{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}
-- compute-worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- With configuration, master and named slave will be assigned with
-- IP address and they automatically find each other.
-- Once ready, master will start a required process as ordered by REST API.
module Main where

import           Control.Distributed.Process.Lifted
                                     ( expect, send )
import           Control.Error.Util  ( failWith )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as HM
import           Data.List           ( find )
import           Data.Semigroup      ( (<>) )
import           Data.Text           ( Text )
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
import           System.FilePath     ( (</>)
                                     , takeDirectory
                                     , takeExtension
                                     )
import           System.INotify      ( Event(..)
                                     , EventVariety(..)
                                     , INotify(..)
                                     , addWatch
                                     , withINotify
                                     )
import           System.IO           ( hPutStrLn, stderr )
-----------------
import           CloudHaskell.Client ( heartBeatHandshake, routerHandshake )
import           CloudHaskell.Type   ( TCPPort(..)
                                     , Gateway(gatewayMaster)
                                     , MasterConfig(..)
                                     , SlaveConfig(..)
                                     , handleError
                                     )
import           CloudHaskell.Util   ( lookupRouter )
-----------------
import           Compute             ( masterMain, slaveMain )
import           Compute.Type        ( ComputeConfig(..)
                                     , NetworkConfig(..)
                                     , CellConfig(..)
                                     )
import           Compute.Type.Status ( Status(..) )


--
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment
import GHC.Hotswap
import Types


data ComputeWorkerOption =
  ComputeWorkerOption
  { servLangConfig :: FilePath
  , servComputeConfig :: FilePath
  }
  deriving (Show,Eq,Ord)


data ProgCommand =
    Master
      ComputeWorkerOption -- ^ options
  | Slave
      Text                -- ^ name
      ComputeWorkerOption -- ^ options
  deriving (Show,Eq,Ord)

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


pCommand :: Parser ProgCommand
pCommand =
  subparser
     ( command "master"
         (info
           (Master <$> pOptions)
           (progDesc "running as master")
         )
    <> command "slave"
         (info
           (Slave  <$> strOption (long "name" <> short 'n' <> help "Cell name")
                   <*> pOptions
           )
           (progDesc "running as slave")
         )
     )


main' :: IO ()
main' =
  handleError $ do
    cmd <- liftIO $ execParser (info (pCommand <**> helper) (progDesc "compute"))
    case cmd of
      Master opt -> do
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
        liftIO $ masterMain initStatus mConfig
      Slave cname opt -> do
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
        liftIO $
          slaveMain (mConfig,sConfig)
            -- TODO: this is not a correct implementation. we should change it.
            (\gw -> do
               heartBeatHandshake (gatewayMaster gw) $
                 routerHandshake $ \router -> do
                   themaster <- lookupRouter "master" router
                   send themaster cname
                   () <- expect -- this is a kill signal.
                   pure ()
            )


looper :: UpdatableSO SOHandles -> IO ()
looper so = do
  threadDelay 2000000
  withSO so $ \SOHandles{..} -> do
    putStrLn $ "someData = " ++ show someData
    someFn 7


notified :: UpdatableSO SOHandles -> FilePath -> Event -> IO ()
notified so basepath e =
 case e of
   Created _ fp_bs -> do
     let fp =  basepath </> B.unpack fp_bs
     when (takeExtension fp == ".o") $ do
       hPutStrLn stderr ("swap SO file: " ++ fp)
       swapSO so fp
   _ -> pure ()


main :: IO ()
main = do
  args <- getArgs
  so_path <- case args of
    [p] -> return p
    _ -> throwIO (ErrorCall "must give file path of first .so as an arg")

  let so_dir = takeDirectory so_path
      so_dir_bs = B.pack (so_dir)

  so <- registerHotswap "hs_soHandles" so_path


  bracket (forkIO (forever $ looper so)) killThread $ \_ -> do
    -- forkIO $
    withINotify $ \inotify -> do
      addWatch inotify [Create] so_dir_bs (notified so so_dir)
      -- idling
      forever $ do
        threadDelay 1000000
        pure ()
