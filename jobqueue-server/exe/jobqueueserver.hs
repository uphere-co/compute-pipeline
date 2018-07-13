{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Distributed.Process       (ProcessId,SendPort,ReceivePort)
import           Control.Distributed.Process.Lifted (getSelfPid,send
                                                    ,newChan,receiveChan,sendChan
                                                    )
import           Control.Distributed.Process.Node  (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                 (bracket)
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Network.Transport                 (closeTransport)
--
import           CloudHaskell.Type                 (LogProcess,Q(..),R(..))
import           CloudHaskell.Util                 (expectSafe
                                                   ,server
                                                   ,tellLog
                                                   ,tryCreateTransport)
import           Network.Transport.UpHere          (DualHostPortPair(..))

import qualified Data.IntMap as M

-- type QR q r = (q, SendPort r)

start :: () -> () -> LogProcess ()
start () () = do
  pid <- getSelfPid
  liftIO $ print pid
  ethem <- lift expectSafe
  case ethem of
    Left err -> tellLog err
    Right (them :: ProcessId) -> do
      tellLog ("got client pid: " ++ show them)
      (sq :: SendPort Q, rq :: ReceivePort Q) <- newChan
      send them sq
      tellLog "sent SendPort Q"
      esr <- lift expectSafe
      case esr of
        Left err' -> tellLog err'
        Right (sr :: SendPort R) -> do
          tellLog "receive SendPortR"
          forever $ do
            q <- receiveChan rq
            tellLog (show q)
            sendChan sr R
            -- ((),sc') <- receiveChan rc
            -- tellLog "received"
            -- sendChan sc' ()

main :: IO ()
main = do
  putStrLn "jobqueueserver"
  let host = "127.0.0.1"
      port = "38833"
      port_broadcast = "38832"
  let dhpp = DHPP (host,port) (host,port)
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport ->
                 newLocalNode transport initRemoteTable
             >>= \node -> runProcess node (server () port_broadcast start ())
          )

  -- acid <- openLocalState (JobInfoQueue 0 M.empty)
  -- sconf <- serverConfigParser "test.conf"
  -- warpDebug 3600 (JobQueueServer acid sconf)

  -- createCheckpoint acid
