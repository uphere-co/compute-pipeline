{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module SemanticParserAPI.Compute.Handler where

import           Control.Concurrent.STM                    ( TVar, readTVarIO )
import           Control.Distributed.Process.Lifted        ( ProcessId
                                                           , SendPort
                                                           , ReceivePort
                                                           , expect
                                                           , send
                                                           , sendChan
                                                           , receiveChan
                                                           )
import           Control.Lens                              ( (^.), to )
import           Control.Monad.IO.Class                    ( liftIO )
import qualified Data.HashMap.Strict                 as HM
--
import           CloudHaskell.Server                       ( serverUnit, withHeartBeat )
import           CloudHaskell.Type                         ( Pipeline, Router(..) )
import           CloudHaskell.Util                         ( RequestDuplex
                                                           , tellLog
                                                           , expectSafe
                                                           , spawnChannelLocalSend
                                                           )
import           Task.CoreNLP                              ( QCoreNLP, RCoreNLP )
import           Task.SemanticParser                       ( ComputeQuery(..)
                                                           , ComputeResult(..)
                                                           )
--
import           SemanticParserAPI.Compute.Type
import           SemanticParserAPI.Compute.Type.Status     ( nodeStatusIsServing
                                                           , nodeStatusNumServed
                                                           , Status
                                                           , StatusQuery(..)
                                                           , StatusResult(..)
                                                           , statusNodes
                                                           )


statusQuery ::
     TVar Status
  -> StatusQuery
  -> Pipeline StatusResult
statusQuery ref _ = do
  m <- liftIO $ readTVarIO ref
  let getStatus x = (x^.nodeStatusIsServing,x^.nodeStatusNumServed)
      lst = map (\(k,v) -> (k,fmap getStatus v)) (m^.statusNodes.to HM.toList)
  pure (SR lst)

requestHandler ::
     TVar Status
  -> RequestDuplex ComputeQuery ComputeResult
  -> RequestDuplex QCoreNLP RCoreNLP
  -> Pipeline ()
requestHandler ref (sq,rr) (sqcorenlp,rrcorenlp) = do
  them_ping :: ProcessId <- expectSafe
  tellLog ("got client ping pid : " ++ show them_ping)
  withHeartBeat them_ping (const (pure ())) $ \them_main -> do

    (slock0,pid0) <-
      spawnChannelLocalSend $ \rlock0 ->
        serverUnit rlock0 $ \q -> do
          sendChan sq q
          receiveChan rr
    (slock1,pid1) <-
      spawnChannelLocalSend $ \rlock1 ->
        serverUnit rlock1 (statusQuery ref)
    (slock2,pid2) <-
      spawnChannelLocalSend $ \rlock2 ->
        serverUnit rlock2 $ \q -> do
          sendChan sqcorenlp q
          receiveChan rrcorenlp
    let router = Router $
                   HM.insert "corenlp" pid2 $
                   HM.insert "test"    pid1 $
                   HM.insert "query"   pid0 $
                   HM.empty
    send them_main router
    sendChan slock0 ()
    sendChan slock1 ()
    sendChan slock2 ()
    () <- expect  -- wait indefinitely
    pure ()
