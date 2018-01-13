{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Options.Applicative
--
import           SemanticParserAPI.CLI.Client (clientMain)
import           SemanticParserAPI.CLI.Type   (clientOption,hostg,hostl,port,serverip,serverport)


main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt
  clientMain (port opt,hostg opt,hostl opt,serverip opt,serverport opt)
