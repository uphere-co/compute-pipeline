{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Task where

import           Control.Concurrent (threadDelay)
import           Control.Distributed.Process.Closure (remotable,mkStatic)
import           Control.Distributed.Process         (Closure,Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (staticDecode)
import           Control.Distributed.Process.Serializable  (SerializableDict(..)
                                                           ,Serializable)
import           Control.Distributed.Static          (closure
                                                     ,registerStatic
                                                     ,staticClosure,staticPtr)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Binary                         (encode)
import           Data.Rank1Dynamic
import           Data.Typeable                       (Typeable)
import           GHC.StaticPtr                       (StaticPtr)
--
import           CloudHaskell.Closure                (StaticSerializableDict(..))
import           CloudHaskell.QueryQueue             (QQVar)
import           CloudHaskell.Util                   (ioWorker
                                                     ,spawnChannelLocalDuplex)
import           Task.CoreNLP                        (QCoreNLP(..),RCoreNLP(..)
                                                     ,daemonCoreNLP)
--
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..)
                                                     ,ComputeResult(..))
import           SemanticParserAPI.Compute.Worker    (runSRLQueryDaemon)


mkRemoteDaemon ::
    (Serializable q, Serializable r) =>
    (QQVar q r -> IO ())
  -> SendPort Bool   -- ^ isServing
  -> SendPort r
  -> ReceivePort q
  -> Process ()
mkRemoteDaemon daemon sstat sr rq = do
  -- Semantic Parser worker daemon
  ((sq_i,rr_i),_) <- spawnChannelLocalDuplex $ \(rq_i,sr_i) ->
    ioWorker (rq_i,sr_i) daemon
  -- Query processing
  forever $ do
    q <- receiveChan rq
    sendChan sstat True   -- serving
    liftIO $ threadDelay 10000000
    sendChan sq_i q
    r <- receiveChan rr_i
    sendChan sr r
    sendChan sstat False  -- not serving

remoteDaemonSemanticParser ::
     (Bool,Bool)
   -> FilePath
   -> SendPort Bool
   -> SendPort ComputeResult
   -> ReceivePort ComputeQuery
   -> Process ()
remoteDaemonSemanticParser (bypassNER,bypassTEXTNER) lcfg =
  mkRemoteDaemon (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)


remoteDaemonCoreNLP ::
     SendPort Bool
  -> SendPort RCoreNLP
  -> ReceivePort QCoreNLP
  -> Process ()
remoteDaemonCoreNLP =
  mkRemoteDaemon daemonCoreNLP

rtable :: RemoteTable
rtable =
  registerStatic
    "$remoteDaemonCoreNLP"
    (toDynamic (staticPtr (static remoteDaemonCoreNLP)))
    initRemoteTable


instance StaticSerializableDict Bool where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict (Bool,Bool) where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict Int where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict String where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict ComputeQuery where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict ComputeResult where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict QCoreNLP where
  staticSdict = staticPtr (static SerializableDict)

instance StaticSerializableDict RCoreNLP where
  staticSdict = staticPtr (static SerializableDict)
