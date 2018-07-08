module Main where

import JobQueue.Server.Yesod
import JobQueue.Server.Work
import JobQueue.JobQueue

import qualified Data.IntMap as M

main :: IO ()
main = do
  putStrLn "jobqueueserver"
  -- acid <- openLocalState (JobInfoQueue 0 M.empty)
  -- sconf <- serverConfigParser "test.conf"
  -- warpDebug 3600 (JobQueueServer acid sconf)

  -- createCheckpoint acid
