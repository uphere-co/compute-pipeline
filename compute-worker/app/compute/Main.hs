{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}
-- compute-worker is the main distributed computing process for a generic
-- task. It has two mode: master and slave.
-- With configuration, master and named slave will be assigned with
-- IP address and they automatically find each other.
-- Once ready, master will start a required process as ordered by REST API.
module Main where

import           Control.Concurrent  ( MVar, ThreadId
                                     , forkIO, killThread
                                     , newEmptyMVar, newMVar, putMVar, takeMVar
                                     )
import           Control.Exception   ( throwIO, ErrorCall(..) )
import           Control.Monad       ( forever, void, when )
import qualified Data.ByteString.Char8 as B
import           GHC.Hotswap        ( UpdatableSO, registerHotswap, swapSO, withSO )
import           Network.Wai.Handler.Warp ( run )
import           System.Environment  ( getArgs )
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
import           Worker.Type         ( SOHandle(..) )
-----------------


looper :: UpdatableSO SOHandle -> IO ()
looper so = do
  visitorCount <- newMVar 0

  withSO so $ \SOHandle{..} ->
    run 3994 $ soApplication visitorCount


notified :: UpdatableSO SOHandle -> FilePath -> MVar () -> ThreadId -> Event -> IO ()
notified so basepath lock tid e =
 case e of
   Created _ fp_bs -> do
     let fp =  basepath </> B.unpack fp_bs
     when (takeExtension fp == ".o") $ do
       hPutStrLn stderr ("swap SO file: " ++ fp)
       killThread tid
       swapSO so fp
       putMVar lock ()
   _ -> pure ()


main :: IO ()
main = do
  putStrLn "start orchestrator"
  args <- getArgs
  so_path <- case args of
    [p] -> return p
    _ -> throwIO (ErrorCall "must give file path of first .so as an arg")

  let so_dir = takeDirectory so_path
      so_dir_bs = B.pack (so_dir)

  so <- registerHotswap "hs_soHandles" so_path

  forever $ do
    tid <- forkIO $ looper so

    withINotify $ \inotify -> do
      lock <- newEmptyMVar
      addWatch inotify [Create] so_dir_bs (notified so so_dir lock tid)
      -- idling
      void $ takeMVar lock
