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
import           CloudHaskell.Closure                (Capture(..))
import           CloudHaskell.QueryQueue             (QQVar)
import           CloudHaskell.Util                   (ioWorker
                                                     ,spawnChannelLocalDuplex)
import           Task.CoreNLP                        (QCoreNLP(..),RCoreNLP(..)
                                                     ,daemonCoreNLP)
--
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..)
                                                     ,ComputeResult(..))
import           SemanticParserAPI.Compute.Worker    (runSRLQueryDaemon)


-- TODO: We need to eliminate these boilerplates!
{-
sdictBool :: SerializableDict Bool
sdictBool = SerializableDict

test :: (Typeable a, Typeable b) => StaticPtr (a -> b -> (a,b))
test = static (,)

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
-}

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

{-
remotable [ 'sdictBool
          , 'sdictBoolBool
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
-}


instance Capture Bool where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture (Bool,Bool) where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture Int where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture String where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture ComputeQuery where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture ComputeResult where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture QCoreNLP where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

instance Capture RCoreNLP where
  capture = closure (staticDecode (staticPtr (static SerializableDict))) . encode
  staticSdict = staticPtr (static SerializableDict)

{-
remoteDaemonSemanticParser__closure ::
  Closure (   (Bool,Bool)
           -> FilePath
           -> SendPort Bool
           -> SendPort ComputeResult
           -> ReceivePort ComputeQuery
           -> Process ())
remoteDaemonSemanticParser__closure = staticClosure $(mkStatic 'remoteDaemonSemanticParser)

remoteDaemonCoreNLP__closure ::
  Closure (   SendPort Bool
           -> SendPort RCoreNLP
           -> ReceivePort QCoreNLP
           -> Process ())
remoteDaemonCoreNLP__closure = staticClosure $(mkStatic 'remoteDaemonCoreNLP)
-}
