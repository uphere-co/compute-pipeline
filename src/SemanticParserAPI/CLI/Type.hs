module SemanticParserAPI.CLI.Type where

import           Options.Applicative
import           Data.Monoid          ((<>))

data ClientOption = ClientOption { port :: Int
                                 , hostg :: String
                                 , hostl :: String
                                 , serverip :: String
                                 , serverport :: Int
                                 } deriving Show

pOptions :: Parser ClientOption
pOptions = ClientOption <$> option auto (long "port" <> short 'p' <> help "Port number")
                        <*> strOption (long "global-ip" <> short 'g' <> help "Global IP address")
                        <*> strOption (long "local-ip" <> short 'l' <> help "Local IP address")
                        <*> strOption (long "server-ip" <> short 's' <> help "Server IP address")
                        <*> option auto (long "server-port" <> short 'q' <> help "Server Port")

clientOption :: ParserInfo ClientOption
clientOption = info pOptions (fullDesc <> progDesc "Client")
