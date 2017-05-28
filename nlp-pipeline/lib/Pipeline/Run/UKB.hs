{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.UKB where

import qualified Data.ByteString.Char8  as B
import           System.FilePath             ((</>))
--
import           HUKB.PPR

runPPR :: String -> IO ()
runPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  print result

getPPR :: String -> IO (B.ByteString,[(B.ByteString, B.ByteString, B.ByteString, B.ByteString)])
getPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  return result
