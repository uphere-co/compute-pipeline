{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Distributed.Process.Node
import           Control.Exception                   (SomeException(..),bracket,try)
import           Control.Monad.Trans.Reader          (runReaderT)
import           Options.Applicative
--
import           Network.Transport                   (closeTransport)
import           Network.Transport.UpHere            (DualHostPortPair(..))
import           Network.Util                        (atomicLog,newLogLock,tryCreateTransport)
--
import           SemanticParserAPI.CLI.Client        (initProcess,retrieveQueryServerPid)
import           SemanticParserAPI.CLI.Type          (clientOption,hostg,hostl,port)




main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt

  let dhpp = DHPP (hostg opt,show (port opt)) (hostl opt,show (port opt))
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport -> do
               node <- newLocalNode transport initRemoteTable
               lock <- newLogLock 0
               emthem <- try (retrieveQueryServerPid lock opt)
               case emthem of
                 Left (e :: SomeException) -> do
                   atomicLog lock "exception caught"
                   atomicLog lock (show e)
                 Right mthem ->
                   case mthem of
                     Nothing -> atomicLog lock "no pid"
                     Just them -> do
                       atomicLog lock (show them)
                       runProcess node (flip runReaderT lock (initProcess them)))

