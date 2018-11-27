{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compute.Task where

import           Control.Concurrent                  ( threadDelay )
import           Control.Concurrent.STM              ( newTVarIO )
import           Control.Distributed.Process         (Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Serializable  (Serializable)
import           Control.Distributed.Static          (registerStatic,staticPtr)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Rank1Dynamic                   (toDynamic)
import           Development.GitRev                  (gitHash)
--
import           CloudHaskell.QueryQueue             (QQVar)
import           CloudHaskell.Util                   (ioWorker
                                                     ,spawnChannelLocalDuplex)
import           Task.CoreNLP                        ( QCoreNLP(..)
                                                     , RCoreNLP(..)
                                                     , daemonCoreNLP
                                                     )
import           Task.SemanticParser                 ( ComputeQuery(..)
                                                     , ComputeResult(..)
                                                     , runSRLQueryDaemon
                                                     )
import           Worker.Type                         ( StatusProc(..) )


mkRemoteDaemon ::
    (Serializable q, Serializable r) =>
    (QQVar q r -> IO ())
  -> SendPort (Bool,Int)   -- ^ isServing
  -> SendPort r
  -> ReceivePort q
  -> Process ()
mkRemoteDaemon daemon sstat sr rq = do
    -- Semantic Parser worker daemon
    ((sq_i,rr_i),_) <- spawnChannelLocalDuplex $ \(rq_i,sr_i) ->
      ioWorker (rq_i,sr_i) daemon
    -- Query processing
    go (sq_i,rr_i) 0
  where
    go (sq_i,rr_i) n = do
      q <- receiveChan rq
      sendChan sstat (True,n)   -- serving
      -- TODO: Remove this test code
      liftIO $ threadDelay 5000000
      sendChan sq_i q
      r <- receiveChan rr_i
      sendChan sr r
      sendChan sstat (False,n+1)  -- not serving
      go (sq_i,rr_i) (n+1)

remoteDaemonSemanticParser ::
     (Bool,Bool)
   -> FilePath
   -> SendPort (Bool,Int)
   -> SendPort ComputeResult
   -> ReceivePort ComputeQuery
   -> Process ()
remoteDaemonSemanticParser (bypassNER,bypassTEXTNER) lcfg sStat sR rQ = do
  rProc <- liftIO $ newTVarIO ProcNone
  mkRemoteDaemon (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg rProc) sStat sR rQ


remoteDaemonCoreNLP ::
     SendPort (Bool,Int)
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

gitRev :: String
gitRev = $(gitHash)
