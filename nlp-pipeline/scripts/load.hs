module Main where

--
import Pipeline.Load

main :: IO ()
main = do
  fps <- getFileListRecursively "/home/modori/temp/mgs"
  list <- loadSRLResult fps
  flip mapM_ list $ \(fp,mmg) -> do
    print mmg
