{-# LANGUAGE OverloadedStrings #-}

module SemanticParserAPI.CLI.Type where

import           Control.Applicative  (optional)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text       as T
import           Options.Applicative

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
