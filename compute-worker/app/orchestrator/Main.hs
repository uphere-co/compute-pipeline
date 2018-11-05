{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.MVar  ( MVar, modifyMVar, newMVar )
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )
import           Network.Wai.Handler.Warp ( run )


application :: MVar Int -> Application
application countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 1102
        msg =    fromByteString (B.pack (show count'))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)

main :: IO ()
main = do
  putStrLn "orchestrator starts"
  ref <- newMVar (0 :: Int)
  run 32929 $ application ref
