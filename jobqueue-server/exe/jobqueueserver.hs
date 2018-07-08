module Main where

import           Control.Concurrent                (threadDelay)
import           Control.Concurrent.STM            (newEmptyTMVarIO)
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Distributed.Process       (Process,getSelfPid)
import           Control.Distributed.Process.Node  (initRemoteTable,newLocalNode,runProcess)
import           Control.Exception                 (bracket)
import           Network.Transport                 (closeTransport)
--
import           CloudHaskell.Util                 (newLogLock,tryCreateTransport)
import           Network.Transport.UpHere          (DualHostPortPair(..))
import           JobQueue.Server.Yesod
import           JobQueue.Server.Work
import           JobQueue.JobQueue

import qualified Data.IntMap as M

serve :: Process ()
serve = do
  pid <- getSelfPid
  liftIO $ print pid
  liftIO $ threadDelay 1000000

server :: Process ()
server = do
  pidref <- liftIO newEmptyTMVarIO
  liftIO $ putStrLn "server started"
  lock <- newLogLock 0
  forever $ serve


main :: IO ()
main = do
  putStrLn "jobqueueserver"
  let host = "127.0.0.1"
      port = "38833"
  let dhpp = DHPP (host,port) (host,port)
  bracket (tryCreateTransport dhpp)
          closeTransport
          (\transport ->
                 newLocalNode transport initRemoteTable
             >>= \node -> runProcess node server
          )

  -- acid <- openLocalState (JobInfoQueue 0 M.empty)
  -- sconf <- serverConfigParser "test.conf"
  -- warpDebug 3600 (JobQueueServer acid sconf)

  -- createCheckpoint acid
