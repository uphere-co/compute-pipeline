{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Distributed.Process (SendPort,ReceivePort)
import Control.Distributed.Process.Lifted (sendChan,receiveChan)
--
import CloudHaskell.Util (LogProcess,Q(..),R(..)
                         ,tellLog
                         ,heartBeatHandshake
                         ,mainP,client)

type QR q r = (q, SendPort r)

start :: (SendPort Q,ReceivePort R) -> LogProcess ()
start (sq,rr) = do
  tellLog "received"
  let single = do
        sendChan sq Q
        r <- receiveChan rr
        tellLog (show r)
  single
  single
  single


main :: IO ()
main = do
  let port = 10290
      host = "127.0.0.1"
      server = "127.0.0.1"
      serverport = 38832
  putStrLn "jobqueueclient"
  client (port,host,host,server,serverport)
         (\them_ping -> heartBeatHandshake them_ping (mainP start))
