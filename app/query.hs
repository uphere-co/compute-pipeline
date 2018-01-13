{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Monoid               ((<>))
import           Options.Applicative
--
import           SemanticParserAPI.Compute (computeMain)


data ComputeServerOption = ComputeServerOption { _port :: Int
                                               , _hostg :: String
                                               , _hostl :: String
                                               -- , _config :: String
                                               -- , _corenlp :: String
                                               }


pOptions :: Parser ComputeServerOption
pOptions = ComputeServerOption
           <$> option auto (long "port" <> short 'p' <> help "Port number")
           <*> strOption (long "global-ip" <> short 'g' <> help "Global IP address")
           <*> strOption (long "local-ip"  <> short 'l' <> help "Local IP address")
           --  <*> strOption (long "config-file" <> short 'c' <> help "Config file")
           --  <*> strOption (long "corenlp" <> short 'n' <> help "CoreNLP server address")


computeServerOption :: ParserInfo ComputeServerOption
computeServerOption = info pOptions ( fullDesc <>
                                      progDesc "semantic-parser-api-compute server daemon" <>
                                      header "options are port, global-ip, local-ip")


main :: IO ()
main = do
  opt <- execParser computeServerOption
  computeMain (_port opt,_hostg opt,_hostl opt)

