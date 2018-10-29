{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}
-- compute-worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- With configuration, master and named slave will be assigned with
-- IP address and they automatically find each other.
-- Once ready, master will start a required process as ordered by REST API.
module Main where

import           Control.Distributed.Process.Lifted    ( expect, send )
import           Control.Error.Util                    ( failWith )
import           Control.Monad.IO.Class                ( liftIO )
import           Control.Monad.Trans.Except            ( ExceptT(..), runExceptT )
import           Data.Aeson                            ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as HM
import           Data.List                             ( find )
import           Data.Semigroup                        ( (<>) )
import           Data.Text                             ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options.Applicative                   ( Parser
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
import           System.IO                             ( stderr )
-----------------
import           CloudHaskell.Client                   ( client
                                                       , heartBeatHandshake
                                                       , routerHandshake
                                                       )
import           CloudHaskell.Type                     ( TCPPort(..)
                                                       , Gateway(gatewayMaster)
                                                       )
import           CloudHaskell.Util                     ( lookupRouter )
-----------------
import           SemanticParserAPI.Compute             ( computeMain )
import           SemanticParserAPI.Compute.Task        ( rtable )
import           SemanticParserAPI.Compute.Type        ( ComputeConfig(..)
                                                       , NetworkConfig(..)
                                                       , CellConfig(..)
                                                       )
import           SemanticParserAPI.Compute.Type.Status ( Status(..) )


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

class RenderError e where
  renderError :: e -> Text

-- TODO: We should refrain from using String error.
instance RenderError String where
  renderError = T.pack

handleError :: (RenderError e) => ExceptT e IO a -> IO ()
handleError m = do
  r <- runExceptT m
  case r of
    Left e -> TIO.hPutStrLn stderr (renderError e)
    Right _ -> pure ()


main :: IO ()
main =
  handleError $ do
    cmd <- liftIO $ execParser (info (pCommand <**> helper) (progDesc "compute"))
    case cmd of
      Master opt -> do
        compcfg :: ComputeConfig <-
          ExceptT $
            eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
        let hostGlobalIP = hostg (computeServer compcfg)
            hostLocalIP = hostl (computeServer compcfg)
            hostPort = port (computeServer compcfg)
            bypassNER = computeBypassNER compcfg
            bypassTEXTNER = computeBypassTEXTNER compcfg
            initStatus =
              Status $
                HM.fromList $
                  map (\c -> (cellName c,Nothing)) (computeCells compcfg)
        liftIO $
          computeMain
            initStatus
            (TCPPort hostPort,hostGlobalIP,hostLocalIP)
            (bypassNER,bypassTEXTNER)
            (servLangConfig opt)
      Slave cname opt -> do
        compcfg :: ComputeConfig
          <- ExceptT $
               eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
        cellcfg <-
          failWith "no such cell" $
            find (\c -> cellName c == cname) (computeCells compcfg)

        let
          cport  = port  (cellAddress cellcfg)
          chostg = hostg (cellAddress cellcfg)
          chostl = hostl (cellAddress cellcfg)
          shostg = hostg (computeServer compcfg)
          sport  = TCPPort (port (computeServer compcfg))
        liftIO $
          client
            rtable
            (cport,chostg,chostl,shostg,sport)
            -- TODO: this is not a correct implementation. we should change it.
            (\gw -> do
               heartBeatHandshake (gatewayMaster gw) $
                 routerHandshake $ \router -> do
                   themaster <- lookupRouter "master" router
                   send themaster cname
                   () <- expect -- this is a kill signal.
                   pure ()
            )
