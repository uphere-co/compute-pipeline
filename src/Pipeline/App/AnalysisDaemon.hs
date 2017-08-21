{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Distributed.Process
import qualified Control.Distributed.Process as Cloud
import           Control.Distributed.Process.Node
import           Control.Exception
import           Control.Monad           (forever,void)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Binary             as Bi
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Char8   as B8
import           Data.Map
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
import           Network.Transport
import           Network.Transport.TCP   (createTransport, defaultTCPParameters)
import           System.Environment
import           Control.Distributed.Process.Node
--                                                                                   
import           Network.Util
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article

nominalDay :: NominalDiffTime
nominalDay = 86400

runDaemon :: IO ()
runDaemon = do
  ctime <- getCurrentTime
  let obday = addUTCTime (-nominalDay) ctime
  getHashByTime obday >>= print
  
{-
  [host, hostB, port, portB] <- getArgs
  pidref              <- newEmptyTMVarIO
  Right transport     <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  config <- loadConfig

  runProcess node $ do
    pid <- spawnLocal $ forever $ do
      (pid :: ProcessId) <- expect
      (query :: Text) <- expect
      liftIO $ print pid
      liftIO $ print query
      Cloud.send pid query

    liftIO $ do
      atomically (putTMVar pidref pid)
      broadcast pidref portB hostB
-}
