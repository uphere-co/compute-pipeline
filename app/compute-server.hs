{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative       (optional)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Options.Applicative
--
import           SemanticParserAPI.Compute (computeMain)


data ComputeServerOption = ComputeServerOption { _port :: Int
                                               , _hostg :: Maybe String
                                               , _hostl :: Maybe String
                                               -- , _config :: String
                                               -- , _corenlp :: String
                                               }


pOptions :: Parser ComputeServerOption
pOptions = ComputeServerOption
           <$> option auto (long "port" <> short 'p' <> help "Port number")
           <*> optional (strOption (long "global-ip" <> short 'g' <> help "Global IP address"))
           <*> optional (strOption (long "local-ip"  <> short 'l' <> help "Local IP address"))
           --  <*> strOption (long "config-file" <> short 'c' <> help "Config file")
           --  <*> strOption (long "corenlp" <> short 'n' <> help "CoreNLP server address")

computeServerOption :: ParserInfo ComputeServerOption
computeServerOption = info pOptions ( fullDesc <>
                                      progDesc "semantic-parser-api-compute server daemon" <>
                                      header "options are port, global-ip, local-ip")


main :: IO ()
main = do
  opt <- execParser computeServerOption
  computeMain (_port opt,fromMaybe "127.0.0.1" (_hostg opt),fromMaybe "127.0.0.1" (_hostl opt))

