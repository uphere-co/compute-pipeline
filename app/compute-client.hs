{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Maybe                   (fromMaybe)
import           Options.Applicative
--
import           CloudHaskell.Util            (initP,mainP,client)
import           SemanticParserAPI.CLI.Client (consoleClient)
import           SemanticParserAPI.CLI.Type   (clientOption,hostg,hostl,port,serverip,serverport)


main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt
  client (port opt
         ,fromMaybe "127.0.0.1" (hostg opt)
         ,fromMaybe "127.0.0.1" (hostl opt)
         ,fromMaybe "127.0.0.1" (serverip opt)
         ,serverport opt)
         (initP (mainP consoleClient))
