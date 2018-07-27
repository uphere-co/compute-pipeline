{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Distributed.Process.Lifted (expect,send)
import           Control.Error.Util                 (failWith)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Except         (ExceptT(..),runExceptT)
import           Data.Aeson                         (eitherDecodeStrict)
import qualified Data.ByteString.Char8         as B
import           Data.List                          (find)
import           Data.Monoid                        ((<>))
import qualified Data.Text                     as T
import           Options.Applicative
--
import           CloudHaskell.Client                (heartBeatHandshake,client,routerHandshake)
import           CloudHaskell.Type                  (TCPPort(..),Gateway(..))
import           CloudHaskell.Util                  (lookupRouter)
--
import           SemanticParserAPI.Compute.Task     (rtable)
import           SemanticParserAPI.Compute.Type     (CellConfig(..)
                                                    ,ComputeConfig(..)
                                                    ,NetworkConfig(..))



data CellOption = CellOption { cellOptComputeConfig :: FilePath
                             , cellOptName          :: String
                             }
                deriving (Show)

cellOption :: ParserInfo CellOption
cellOption =
  info
    (CellOption <$> strOption (long "compute" <> short 'c' <> help "Compute pipeline configuration")
                <*> strOption (long "name" <> short 'n' <> help "Cell name"))
    (fullDesc <> progDesc "Cell")

main :: IO ()
main = do
  opt <- execParser cellOption
  r <- runExceptT $ do
    compcfg :: ComputeConfig <- ExceptT $ eitherDecodeStrict <$> B.readFile (cellOptComputeConfig opt)
    let cname = T.pack (cellOptName opt)
    cellcfg <- failWith "no such cell" $ find (\c -> cellName c == cname) (computeCells compcfg)
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
