{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Distributed.Process.Node
import qualified Control.Exception                  as Ex
import           Control.Monad.Trans.Reader               (runReaderT)
import           Options.Applicative
--
import           Network.Transport              (closeTransport)
import           Network.Transport.UpHere       (createTransport
                                                ,defaultTCPParameters
                                                ,DualHostPortPair(..))
import           Network.Util
--
import           SemanticParserAPI.CLI.Client   (initProcess,retrieveQueryServerPid)
import           SemanticParserAPI.CLI.Type     (clientOption,hostg,hostl,port)




main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt

  let dhpp = DHPP (hostg opt,show (port opt)) (hostl opt,show (port opt))
  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      lock <- newLogLock 0
      emthem <- Ex.try (retrieveQueryServerPid lock opt)
      case emthem of
        Left (e :: Ex.SomeException) -> do
          atomicLog lock "exception caught"
          atomicLog lock (show e)
          closeTransport transport
        Right mthem ->
          case mthem of
            Nothing -> atomicLog lock "no pid"
            Just them -> do
              atomicLog lock (show them)
              runProcess node (flip runReaderT lock (initProcess them))
              closeTransport transport
