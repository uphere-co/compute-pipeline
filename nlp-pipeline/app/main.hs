module Main where

import Control.Monad   (mapM_)
import System.FilePath ((</>))
--
import HUKB.PPR
--
import Pipeline.Application.Run


main :: IO ()
main = run


{-
main :: IO ()
main = do
  putStrLn "test HUKB"
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
      sample1 = "man#n#w1#1 kill#v#w2#1 cat#n#w3#1 hammer#n#w4#1"
      sample2 = "Tesla#n#w1#1 Inc.#n#w2#1 rally#n#w3#1 be#v#w4#1 give#v#w5#1 electric-car#a#w6#1 maker#n#w7#1 opportunity#n#w8#1 wipe#v#w9#1 out#r#w10#1 substantial#a#w11#1 debt#n#w12#1"
  mapM_ (\x -> ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" sample2) (replicate 10 0)
-}
