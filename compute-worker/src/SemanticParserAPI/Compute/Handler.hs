{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemanticParserAPI.Compute.Handler where

import           Control.Concurrent.STM                    (TVar,readTVarIO)
import           Control.Distributed.Process.Lifted        (ProcessId,SendPort,ReceivePort
                                                           ,expect,send
                                                           ,sendChan,receiveChan)
import           Control.Lens                              ((^.),to)
import           Control.Monad.IO.Class                    (liftIO)
import qualified Data.HashMap.Strict                 as HM
--
import           CloudHaskell.Server                       (serverUnit,withHeartBeat)
import           CloudHaskell.Type                         (Pipeline,Router(..))
import           CloudHaskell.Util                         (tellLog
                                                           ,expectSafe
                                                           ,spawnChannelLocalSend)
--
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..)
                                                           ,ComputeResult(..))
import           SemanticParserAPI.Compute.Type.Status     (nodeStatusIsServing
                                                           ,Status
                                                           ,StatusQuery(..)
                                                           ,StatusResult(..)
                                                           ,statusNodes)


statusQuery ::
     TVar Status
  -> StatusQuery
  -> Pipeline StatusResult
statusQuery ref _ = do
  m <- liftIO $ readTVarIO ref
  let lst = map (\(k,v) -> (k,fmap (^.nodeStatusIsServing) v)) (m^.statusNodes.to HM.toList)
  pure (SR lst)

requestHandler ::
     TVar Status
  -> (SendPort ComputeQuery, ReceivePort ComputeResult)
  -> Pipeline ()
requestHandler ref (sq,rr) = do
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


    let router = Router $
                   HM.insert "test"  pid1 $
                   HM.insert "query" pid0 $
                   HM.empty
    send them_main router
    sendChan slock0 ()
    sendChan slock1 ()
    () <- expect  -- wait indefinitely
    pure ()

