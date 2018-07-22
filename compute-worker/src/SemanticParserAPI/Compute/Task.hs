{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable)
import           Control.Distributed.Process         (Process,ProcessId,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,send,sendChan,receiveChan)
-- import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Monad.IO.Class (liftIO)

import Control.Distributed.Static


sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict


{-
launchMissile :: ProcessId -> Process ()
launchMissile pid = do
  liftIO $ putStrLn "nuclear launch detected!"
  send pid (3 :: Int)
-}

holdState :: String -> Int {- -> ReceivePort Int -} -> Process ()
holdState p sr {- rq  -} = do
  liftIO $ putStrLn p
  -- i <- receiveChan rq
  -- sendChan sr (i+1)
  liftIO $ print sr
  -- pure "Abc"



-- remotable ['launchMissile]

remotable [ 'holdState
          , 'sdictInt
          , 'sdictString
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable
