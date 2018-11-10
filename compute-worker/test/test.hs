module Main where

import Control.Concurrent        ( threadDelay )
import Control.Monad             ( forever )
import Network.WebSockets.Client ( receive, withConnection )

main :: IO ()
main = do
  withConnection "ws://localhost:3123/stream" $ \conn ->
    forever $  do
      x <- receive conn
      print x
      -- threadDelay 1000000
