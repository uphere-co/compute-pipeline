{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# OPTIONS_GHC -w #-}
-- compute-worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- With configuration, master and named slave will be assigned with
-- IP address and they automatically find each other.
-- Once ready, master will start a required process as ordered by REST API.
module Main where

import qualified Data.ByteString.Char8 as B
import           System.FilePath     ( (</>)
                                     , takeDirectory
                                     , takeExtension
                                     )
import           System.INotify      ( Event(..)
                                     , EventVariety(..)
                                     , addWatch
                                     , withINotify
                                     )
import           System.IO           ( hPutStrLn, stderr )
-----------------
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment
import GHC.Hotswap
import Types
-----------------
import Blaze.ByteString.Builder (fromByteString)
import Network.HTTP.Types (status200)
import Network.Wai (responseBuilder)
import Network.Wai.Handler.Warp (run)


looper :: UpdatableSO SOHandles -> IO ()
looper so = do
  visitorCount <- newMVar 0

  withSO so $ \SOHandles{..} ->
    run 3994 $ someApplication visitorCount


notified :: UpdatableSO SOHandles -> FilePath -> Event -> IO ()
notified so basepath e =
 case e of
   Created _ fp_bs -> do
     let fp =  basepath </> B.unpack fp_bs
     when (takeExtension fp == ".o") $ do
       hPutStrLn stderr ("swap SO file: " ++ fp)
       swapSO so fp
   _ -> pure ()


main :: IO ()
main = do
  args <- getArgs
  so_path <- case args of
    [p] -> return p
    _ -> throwIO (ErrorCall "must give file path of first .so as an arg")

  let so_dir = takeDirectory so_path
      so_dir_bs = B.pack (so_dir)

  so <- registerHotswap "hs_soHandles" so_path


  bracket (forkIO (forever $ looper so)) killThread $ \_ -> do
    withINotify $ \inotify -> do
      addWatch inotify [Create] so_dir_bs (notified so so_dir)
      -- idling
      forever $ do
        threadDelay 1000000
        pure ()
