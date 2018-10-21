{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
--
import           CloudHaskell.Client                   ( client
                                                       , heartBeatHandshake
                                                       , routerHandshake
                                                       )
import           CloudHaskell.Type                     ( TCPPort(..)
                                                       , Gateway(gatewayMaster)
                                                       )
import           CloudHaskell.Util                     ( lookupRouter )
--
import           SemanticParserAPI.Compute             ( computeMain )
import           SemanticParserAPI.Compute.Task        ( rtable )
import           SemanticParserAPI.Compute.Type        ( ComputeConfig(..)
                                                       , NetworkConfig(..)
                                                       , CellConfig(..)
                                                       )
import           SemanticParserAPI.Compute.Type.Status ( Status(..) )


data ComputeServerOption = ComputeServerOption {
                             servLangConfig :: FilePath
                           , servComputeConfig :: FilePath
                           }
                         deriving (Show,Eq,Ord)


data ProgCommand = Master
                     ComputeServerOption -- ^ options
                 | Slave
                     Text                -- ^ name
                     ComputeServerOption -- ^ options
                 deriving (Show,Eq,Ord)

pOptions :: Parser ComputeServerOption
pOptions = ComputeServerOption
           <$> strOption (long "lang" <> short 'l' <> help "Language engine configuration")
           <*> strOption (long "compute" <> short 'c' <> help "Compute pipeline configuration")


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


main :: IO ()
main = do
  cmd <- execParser (info (pCommand <**> helper) (progDesc "compute-pipeline daemon"))

  case cmd of
    Master opt -> do
      r <- runExceptT $ do
        compcfg :: ComputeConfig <-
          ExceptT $
            eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
        let hostGlobalIP = hostg (computeServer compcfg)
            hostLocalIP = hostl (computeServer compcfg)
            hostPort = port (computeServer compcfg)
            bypassNER = computeBypassNER compcfg
            bypassTEXTNER = computeBypassTEXTNER compcfg
            initStatus = Status (HM.fromList $ map (\c -> (cellName c,Nothing))  (computeCells compcfg))
        liftIO $
          computeMain
            initStatus
            (TCPPort hostPort,hostGlobalIP,hostLocalIP)
            (bypassNER,bypassTEXTNER)
            (servLangConfig opt)
      case r of
        Left e -> print e
        Right _ -> pure ()

    Slave cname opt -> do
      r <- runExceptT $ do
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
      case r of
        Left e -> print e
        Right _ -> pure ()
