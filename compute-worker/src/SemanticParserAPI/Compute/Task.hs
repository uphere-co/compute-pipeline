{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable,mkStatic)
import           Control.Distributed.Process         (Closure,Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (staticDecode)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Distributed.Static          (closure,staticClosure)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Binary                         (encode)
--
import           CloudHaskell.Closure                (Capture(..))


sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict

sdictSendPortInt :: SerializableDict (SendPort Int)
sdictSendPortInt = SerializableDict


holdState :: Int -> SendPort Int -> ReceivePort Int -> Process ()
holdState s sr rq = go s
  where
    go s0 = do
      i <- receiveChan rq
      liftIO $ putStrLn ("current state = " ++ show s0)
      liftIO $ putStrLn ("received: " ++ show i)
      let s' = s0 + i
      liftIO $ putStrLn ("new state = " ++ show s')
      sendChan sr s'
      go s'


remotable [ 'holdState
          , 'sdictInt
          , 'sdictString
          , 'sdictSendPortInt
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode
  staticSdict = $(mkStatic 'sdictString)

instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode
  staticSdict = $(mkStatic 'sdictInt)


holdState__closure :: Closure (Int -> SendPort Int -> ReceivePort Int -> Process ())
holdState__closure = staticClosure $(mkStatic 'holdState)
