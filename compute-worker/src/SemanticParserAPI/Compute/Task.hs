{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable,mkStatic)
import           Control.Distributed.Process         (Closure,Process,ProcessId,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,send,sendChan,receiveChan)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (staticDecode)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Distributed.Static (closure,staticClosure,initRemoteTable)
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary (encode)
--
import           CloudHaskell.Closure (Capture(..),(@<))

sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict


holdState :: String -> Int {- -> ReceivePort Int -} -> Process ()
holdState p sr {- rq  -} = do
  liftIO $ putStrLn p
  -- i <- receiveChan rq
  -- sendChan sr (i+1)
  liftIO $ print sr
  -- pure "Abc"


remotable [ 'holdState
          , 'sdictInt
          , 'sdictString
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode

instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode


holdState__closure :: Closure (String -> Int -> Process ())
holdState__closure = staticClosure $(mkStatic 'holdState)
