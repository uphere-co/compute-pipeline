{-# LANGUAGE TemplateHaskell #-}
module SemanticParserAPI.Compute.Task where

import Control.Distributed.Process         (Process,ProcessId,RemoteTable,send)
import Control.Distributed.Process.Closure (remotable)
import Control.Distributed.Process.Node    (initRemoteTable)
import Control.Monad.IO.Class (liftIO)

launchMissile :: ProcessId -> Process ()
launchMissile pid = do
  liftIO $ putStrLn "nuclear launch detected!"
  send pid (3 :: Int)

remotable ['launchMissile]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
