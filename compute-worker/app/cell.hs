{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Control.Applicative  (optional)
import           Control.Distributed.Process.Lifted (expect,getSelfPid)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid          ((<>))
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text       as T
import           Options.Applicative
--
import           CloudHaskell.Client  (heartBeatHandshake,client)
import           CloudHaskell.Type    (TCPPort(..),Gateway(..))
import           CloudHaskell.Util    (tellLog)
--
import           SemanticParserAPI.Compute.Task (rtable)


data ClientOption = ClientOption { port :: Int
                                 , hostg :: Maybe Text
                                 , hostl :: Maybe Text
                                 , serverip :: Maybe Text
                                 , serverport :: Int
                                 } deriving Show

pOptions :: Parser ClientOption
pOptions = ClientOption <$> option auto (long "port" <> short 'p' <> help "Port number")
                        <*> (fmap T.pack <$> optional (strOption (long "global-ip" <> short 'g' <> help "Global IP address")))
                        <*> (fmap T.pack <$> optional (strOption (long "local-ip" <> short 'l' <> help "Local IP address")))
                        <*> (fmap T.pack <$> optional (strOption (long "server-ip" <> short 's' <> help "Server IP address")))
                        <*> option auto (long "server-port" <> short 'q' <> help "Server Port")

clientOption :: ParserInfo ClientOption
clientOption = info pOptions (fullDesc <> progDesc "Client")

main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt
  client
    rtable
    (port opt
    ,fromMaybe "127.0.0.1" (hostg opt)
    ,fromMaybe "127.0.0.1" (hostl opt)
    ,fromMaybe "127.0.0.1" (serverip opt)
    ,TCPPort (serverport opt))
    -- TODO: this is not a correct implementation. we should change it.
    (\gw -> do
       let them_ping = gatewayMaster gw
       -- liftIO $ print gw
       heartBeatHandshake them_ping $ do
         -- us <- getSelfPid
         -- tellLog ("send our pid: " ++ show us)
         () <- expect -- this is a kill signal.
         pure ()
         -- (serviceHandshake them_ping consoleClient)
    )
