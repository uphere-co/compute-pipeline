module Main where

import System.FilePath ((</>))
--
import HUKB.PPR
--
import Pipeline.Application.Run

main' :: IO ()
main' = run2

main :: IO ()
main = do
  putStrLn "test HUKB"
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" "man#n#w1#1 kill#v#w2#1 cat#n#w3#1 hammer#n#w4#1"

