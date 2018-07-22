{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable)
import           Control.Distributed.Process         (Process,ProcessId,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,send,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Monad.IO.Class (liftIO)

sdictInt :: SerializableDict Int
sdictInt = SerializableDict


{-
launchMissile :: ProcessId -> Process ()
launchMissile pid = do
  liftIO $ putStrLn "nuclear launch detected!"
  send pid (3 :: Int)
-}

holdState :: Int {- -> ReceivePort Int -} -> Process ()
holdState sr {- rq  -} = do
  -- i <- receiveChan rq
  -- sendChan sr (i+1)
  liftIO $ print sr
  pure ()



-- remotable ['launchMissile]

remotable [ 'holdState
          , 'sdictInt
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
