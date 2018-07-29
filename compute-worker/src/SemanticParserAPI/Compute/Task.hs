{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable,mkStatic)
import           Control.Distributed.Process         (Closure,Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (staticDecode)
import           Control.Distributed.Process.Serializable  (SerializableDict(..)
                                                           ,Serializable)
import           Control.Distributed.Static          (closure,staticClosure)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Binary                         (encode)
--
import           CloudHaskell.Closure                (Capture(..))
import           CloudHaskell.QueryQueue             (QQVar)
import           CloudHaskell.Util                   (ioWorker
                                                     ,spawnChannelLocalDuplex)
import           Task.CoreNLP                        (QCoreNLP(..),RCoreNLP(..)
                                                     ,daemonCoreNLP)
--
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..)
                                                           ,ComputeResult(..))
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)


sdictBoolBool :: SerializableDict (Bool,Bool)
sdictBoolBool = SerializableDict

sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict

sdictComputeQuery :: SerializableDict ComputeQuery
sdictComputeQuery = SerializableDict

sdictComputeResult :: SerializableDict ComputeResult
sdictComputeResult = SerializableDict

sdictQCoreNLP :: SerializableDict QCoreNLP
sdictQCoreNLP = SerializableDict

sdictRCoreNLP :: SerializableDict RCoreNLP
sdictRCoreNLP = SerializableDict


mkRemoteDaemon ::
    (Serializable q, Serializable r) =>
    (QQVar q r -> IO ())
  -> SendPort r
  -> ReceivePort q
  -> Process ()
mkRemoteDaemon daemon sr rq = do
  -- Semantic Parser worker daemon
  ((sq_i,rr_i),_) <- spawnChannelLocalDuplex $ \(rq_i,sr_i) ->
    ioWorker (rq_i,sr_i) daemon
  -- Query processing
  forever $ do
    q <- receiveChan rq
    -- liftIO $ putStrLn ("query received: " ++ show q)
    sendChan sq_i q
    r <- receiveChan rr_i
    sendChan sr r


remoteDaemonSemanticParser ::
     (Bool,Bool)
   -> FilePath
   -> SendPort ComputeResult
   -> ReceivePort ComputeQuery
   -> Process ()
remoteDaemonSemanticParser (bypassNER,bypassTEXTNER) lcfg =
  mkRemoteDaemon (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)


remoteDaemonCoreNLP :: SendPort RCoreNLP -> ReceivePort QCoreNLP -> Process ()
remoteDaemonCoreNLP = mkRemoteDaemon daemonCoreNLP


remotable [ 'sdictBoolBool
          , 'sdictInt
          , 'sdictString
          , 'sdictComputeQuery
          , 'sdictComputeResult
          , 'sdictQCoreNLP
          , 'sdictRCoreNLP
          , 'remoteDaemonSemanticParser
          , 'remoteDaemonCoreNLP
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


instance Capture (Bool,Bool) where
  capture = closure (staticDecode $(mkStatic 'sdictBoolBool)) . encode
  staticSdict = $(mkStatic 'sdictBoolBool)


instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode
  staticSdict = $(mkStatic 'sdictInt)

instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode
  staticSdict = $(mkStatic 'sdictString)

instance Capture ComputeQuery where
  capture = closure (staticDecode $(mkStatic 'sdictComputeQuery)) . encode
  staticSdict = $(mkStatic 'sdictComputeQuery)

instance Capture ComputeResult where
  capture = closure (staticDecode $(mkStatic 'sdictComputeResult)) . encode
  staticSdict = $(mkStatic 'sdictComputeResult)

instance Capture QCoreNLP where
  capture = closure (staticDecode $(mkStatic 'sdictQCoreNLP)) . encode
  staticSdict = $(mkStatic 'sdictQCoreNLP)

instance Capture RCoreNLP where
  capture = closure (staticDecode $(mkStatic 'sdictRCoreNLP)) . encode
  staticSdict = $(mkStatic 'sdictRCoreNLP)


remoteDaemonSemanticParser__closure :: Closure ((Bool,Bool) -> FilePath -> SendPort ComputeResult -> ReceivePort ComputeQuery -> Process ())
remoteDaemonSemanticParser__closure = staticClosure $(mkStatic 'remoteDaemonSemanticParser)

remoteDaemonCoreNLP__closure :: Closure (SendPort RCoreNLP -> ReceivePort QCoreNLP -> Process ())
remoteDaemonCoreNLP__closure = staticClosure $(mkStatic 'remoteDaemonCoreNLP)
