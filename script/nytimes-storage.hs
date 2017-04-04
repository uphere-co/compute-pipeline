{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Codec.Archive.Tar          as Tar
import           Codec.Compression.GZip     as GZ
import qualified Data.ByteString.Lazy.Char8 as BL
import           System.Directory
import           System.Environment                   (getArgs)
import           System.FilePath                      (splitFileName)

worker dir (Tar.Next e n) =
  case Tar.entryContent e of
    Tar.NormalFile bstr _ -> do
      let (_,fname) = splitFileName (Tar.entryPath e)
      storeFile dir fname bstr
      worker dir n
    _ -> worker dir n
worker _ _ = return ()                          

-- createDirectoryIfNotExist dir = 

storeFile dir filename bstr = do
  cwd <- getCurrentDirectory 
  setCurrentDirectory dir
  let prefix = take 2 filename
  createDirectoryIfMissing False prefix
  setCurrentDirectory prefix
  -- createDirectoryIfMissing False "abc"
  BL.writeFile filename bstr
  setCurrentDirectory cwd

  
  

main :: IO ()
main = do
  args <- getArgs
  bstr <- BL.readFile (args !! 0)
  let entries = (Tar.read . GZ.decompress) bstr
      dir = args !! 1 
  worker dir entries

  
