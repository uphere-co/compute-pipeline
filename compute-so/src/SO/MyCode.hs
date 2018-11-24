{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SO.MyCode
  ( myApp
  , workerMain
  ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent       ( ThreadId, forkFinally, threadDelay )
import           Control.Concurrent.MVar  ( MVar, modifyMVar )
import           Control.Concurrent.STM   ( TMVar, TVar
                                          , atomically, newTVarIO
                                          , putTMVar
                                          , readTVarIO, writeTVar
                                          )
import           Control.Distributed.Process ( ProcessId )
import           Control.Distributed.Process.Lifted
                                          ( Process, expect, getSelfPid, liftIO, send )
import           Control.Distributed.Process.Node
                                          ( LocalNode, closeLocalNode
                                          , newLocalNode, runProcess
                                          )
import           Control.Exception        ( bracket )
import           Control.Monad            ( forever, void )
import           Control.Monad.Trans.Except ( runExceptT )
import           Control.Monad.Trans.Reader ( runReaderT )
import qualified Data.ByteString.Char8 as B
import           Data.Foldable            ( traverse_ )
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
import qualified Data.Text             as T
import           Network.HTTP.Types       ( status200 )
import           Network.Transport        ( Transport, closeTransport )
import           Network.Wai              ( Application, responseBuilder )
import           System.IO                ( hPutStrLn, stderr )
------
import           CloudHaskell.Client      ( heartBeatHandshake )
import           CloudHaskell.Server      ( withHeartBeat )
import           CloudHaskell.Util        ( expectSafe
                                          , handleErrorLog
                                          , newLogLock
                                          , onesecond
                                          , onKill
                                          , tellLog
                                          , tryCreateTransport
                                          )
import           CloudHaskell.Type        ( Pipeline )
import           Network.Transport.UpHere ( DualHostPortPair(..) )
import           Compute.Task             ( rtable )
------
import           Worker.Type              ( WorkerRole(..)
                                          , CellConfig(..)
                                          , NetworkConfig(..)
                                          )

------
import           SO.Process               ( mainProcess )

myApp :: MVar Int -> Application
myApp countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 12345
        msg =    fromByteString (B.pack (show count'))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)


master :: TMVar ProcessId -> Pipeline ()
master ref = do
  self <- getSelfPid
  tellLog ("master self pid = " ++ show self)
  liftIO $ atomically $ putTMVar ref self

  them_ping :: ProcessId <- expectSafe
  tellLog ("got slave ping pid: " ++ show them_ping)
  withHeartBeat them_ping (\_ -> pure ()) $ \them_main -> do
    mainProcess
    () <- expect  -- for idling
    pure ()


slave :: TMVar ProcessId -> ProcessId -> Pipeline ()
slave ref mpid = do
  heartBeatHandshake mpid $ do
    () <- expect
    pure ()


withTransport :: DualHostPortPair -> (Transport -> IO a) -> IO a
withTransport dhpp action =
  bracket
    (tryCreateTransport dhpp)
    closeTransport
    action

mkDHPP :: NetworkConfig -> DualHostPortPair
mkDHPP cfg = DHPP
                  (T.unpack (hostg cfg), show (port cfg))
                  (T.unpack (hostg cfg), show (port cfg))



killLocalNode :: TVar (Maybe LocalNode) -> IO ()
killLocalNode ref_node = do
  mnode <- readTVarIO ref_node
  traverse_ closeLocalNode mnode

-- NOTE: This should be asynchronous task, i.e. it forks a thread
--       and return the id of the thread.
workerMain :: TMVar ProcessId -> (WorkerRole,CellConfig) -> IO ThreadId
workerMain ref (Master name, mcellcfg) = do
  let dhpp = mkDHPP (cellAddress mcellcfg)
  ref_node <- newTVarIO Nothing
  flip forkFinally (onKill (putStrLn "killed" >> killLocalNode ref_node)) $
    withTransport dhpp $ \transport -> do
      putStrLn "transport started"
      node <- newLocalNode transport rtable
      atomically $ writeTVar ref_node (Just node)
      lock <- newLogLock 0
      runProcess node $
        flip runReaderT lock $
          handleErrorLog $
            master ref
workerMain ref (Slave name mpid, scellcfg) = do
  let dhpp = mkDHPP (cellAddress scellcfg)
  ref_node <- newTVarIO Nothing
  flip forkFinally (onKill (putStrLn "killed" >> killLocalNode ref_node)) $
    withTransport dhpp $ \transport -> do
      putStrLn "transport started"
      node <- newLocalNode transport rtable
      atomically $ writeTVar ref_node (Just node)
      lock <- newLogLock 0
      runProcess node $
        flip runReaderT lock $
          handleErrorLog $
            slave ref mpid
