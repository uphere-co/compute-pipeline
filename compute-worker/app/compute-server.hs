{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative       (optional)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Options.Applicative
--
import           CloudHaskell.Type         (TCPPort(..))
--
import           SemanticParserAPI.Compute (computeMain)


data ComputeServerOption = ComputeServerOption { _port :: Int
                                               , _hostg :: Maybe String
                                               , _hostl :: Maybe String
                                               , _bypassNER :: Bool
                                               , _bypassTEXTNER :: Bool
                                               , _lcfg :: FilePath
                                               }


pOptions :: Parser ComputeServerOption
pOptions = ComputeServerOption
           <$> option auto (long "port" <> short 'p' <> help "Port number")
           <*> optional (strOption (long "global-ip" <> short 'g' <> help "Global IP address"))
           <*> optional (strOption (long "local-ip"  <> short 'l' <> help "Local IP address"))
           <*> switch (long "bypassner" <> short 'n' <> help "bypass NER")
           <*> switch (long "bypasstextner" <> short 't' <> help "bypass TEXTNER")
           <*> strOption (long "lexiconconfig" <> short 'c' <> help "lexicon config")

computeServerOption :: ParserInfo ComputeServerOption
computeServerOption = info pOptions ( fullDesc <>
                                      progDesc "semantic-parser-api-compute server daemon" <>
                                      header "options are port, global-ip, local-ip")


main :: IO ()
main = do
  opt <- execParser computeServerOption
  computeMain
    (TCPPort (_port opt),fromMaybe "127.0.0.1" (_hostg opt),fromMaybe "127.0.0.1" (_hostl opt))
    (_bypassNER opt,_bypassTEXTNER opt)
    (_lcfg opt)

