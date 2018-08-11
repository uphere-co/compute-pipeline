{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SemanticParserAPI.Compute.Task where

import           Control.Concurrent (threadDelay)
import           Control.Distributed.Process         (Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Serializable  (Serializable)
import           Control.Distributed.Static          (registerStatic,staticPtr)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Rank1Dynamic                   (toDynamic)
--
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
    -- TODO: Remove this test code
    liftIO $ threadDelay 5000000
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


