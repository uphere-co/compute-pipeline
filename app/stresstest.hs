{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Distributed.Process.Lifted
import           Control.Distributed.Process.Node
import qualified Control.Exception                  as Ex
import           Control.Monad                            (void)
import           Control.Monad.Trans.Reader               (runReaderT)
import qualified Data.ByteString.Lazy.Char8         as BL
import           Data.Foldable                            (forM_)
import           Data.Monoid                              ((<>))
import           Data.Text                                (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import           Options.Applicative
--
import           CloudHaskell.Server
import           Network.Transport.UpHere       ( createTransport
                                                , defaultTCPParameters
                                                , DualHostPortPair(..))
import           Network.Util
import           QueryServer.Type
--
import           SemanticParserAPI.CLI.Client
import           SemanticParserAPI.CLI.Type

examples =
  take 1000 $
    cycle  [ "Start-up that google bought"
           , "China fire regulation"
           , "China RoHS"  
           , "What is the start-up company that values most in 2016?"
           , "How many start-up went IPO"
           , "Fraud detection using deep learning"
           , "The most breathtaking piano pieces"
           , "The biggest oil spill in the history"
           , "Quality control of autodriving car"
           , "AI will take all human jobs"
           , "Deep learning companies who got funded recently"
           , "Financial industry crisis in 2017"
           , "Google bought a company"
           , "startup that google bought recently"
           , "Successive funding in Series A round"
           , "legislation unit in United States"
           , "affordable chair for disabled person"
           , "doctor wants to see patient"
           , "new observation in pluto"
           , "pluto loses planet privilege"
           , "pluto loses planet"
           , "cross fire"
           ]
          

initProcess2 :: ProcessId -> LogProcess ()
initProcess2 them = do
  us <- getSelfPid
  tellLog ("we are " ++ show us)
  send them us
  void (mainProcess2 them)

mainProcess2 :: ProcessId -> LogProcess ()
mainProcess2 them = do
  tellLog "mainProcess started"
  msc :: Maybe (SendPort (Query,SendPort BL.ByteString)) <- expectTimeout 5000000
  case msc of
    Nothing -> tellLog "cannot receive query port"
    Just sc -> do
      tellLog "connection stablished to query server"
      p1 <- spawnLocal (batchClient examples sc)
      void $ pingHeartBeat p1 them 0   

batchClient :: [Text] -> SendPort (Query, SendPort BL.ByteString) -> LogProcess ()
batchClient qs sc = do
  forM_ qs $ \input' -> do
    liftIO $ TIO.putStrLn (input' <> ": send query")
    
    liftIO $ threadDelay 500000
    spawnLocal $ queryProcess sc (QueryText input' []) $ \bstr -> do
      liftIO $ TIO.putStrLn (input' <> ": answered")
      



main :: IO ()
main = do
  opt <- execParser clientOption
  putStrLn "client"
  print opt

  let dhpp = DHPP (hostg opt,show (port opt)) (hostl opt,show (port opt))
  Right transport <- createTransport dhpp defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  lock <- newLogLock 0
  emthem <- Ex.try (retrieveQueryServerPid lock opt)
  case emthem of
    Left (e :: Ex.SomeException) -> do
      atomicLog lock "exception caught"
      atomicLog lock (show e)
    Right mthem -> 
      case mthem of
        Nothing -> atomicLog lock "no pid"  
        Just them -> do
          atomicLog lock (show them)
          runProcess node . flip runReaderT lock $ do
            -- spawnLocal (initProcess them)
            -- liftIO $ threadDelay 10000000
            initProcess2 them
            
