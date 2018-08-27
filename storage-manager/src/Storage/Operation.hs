{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Storage.Operation where

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Compression.GZip     as GZip
import           Control.Error.Util                (hoistEither)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (ExceptT)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Base16     as B16
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Digest.Pure.MD5              (md5)
import           Data.Semigroup                    ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.UUID                         (toString)
import           Data.UUID.V4                      (nextRandom)
import           System.Directory                  (createDirectory)
import           System.Directory.Tree             (AnchoredDirTree(..),DirTree(..)
                                                   ,dirTree,flattenDir,readDirectory)
import           System.FilePath                   ((</>))
--
import           Storage.Config                    (StorageConfig(..))

mkFileList :: FilePath -> DirTree a -> [FilePath]
mkFileList fp (Dir n cs)    = (fp </> n) : concatMap (mkFileList (fp </> n)) cs
mkFileList fp (File n _)    = [fp </> n]
mkFileList _  (Failed _  _) = []

mkManifest :: DirTree a -> Text
mkManifest d = T.unlines (map T.pack (mkFileList "" d))

tarGz :: AnchoredDirTree a -> ExceptT String IO BL.ByteString
tarGz adir = do
  dir <- hoistEither $
           case dirTree adir of
             Dir n _ -> Right n
             _       -> Left "not a directory"
  lift (GZip.compress . Tar.write <$> Tar.pack (anchor adir) [dir])


calcMD5sum :: FilePath -> IO String
calcMD5sum fp = show . md5 <$> BL.readFile fp


register :: StorageConfig -> FilePath -> ExceptT String IO ()
register cfg fp = do
  liftIO $ putStrLn "register"
  uuid <- liftIO nextRandom
  --
  let pkgpath = storagePath cfg </> toString uuid
  liftIO $ putStrLn $ "new id = " <> pkgpath <> "\ncreate the directory with the uuid as path\n"
  liftIO $ createDirectory pkgpath
  dir <- liftIO $ readDirectory fp
  --
  let manifest = pkgpath </> "MANIFEST"
      manifestTxt = mkManifest (dirTree dir)
  liftIO $ TIO.writeFile manifest manifestTxt
  --
  let readme = pkgpath </> "README.md"
  liftIO $ TIO.writeFile readme ""
  --
  let contentsTarGz = pkgpath </> "contents.tar.gz"
  lbs <- tarGz dir
  liftIO $ BL.writeFile contentsTarGz lbs
  -- calculate md5 hash. to save memory, we reread contetns.tar.gz.
  let md5sum = pkgpath </> "MD5SUM"
  liftIO $ do
    hshstr <- calcMD5sum contentsTarGz
    TIO.writeFile md5sum (T.pack (hshstr <> "  contents.tar.gz\n"))


install :: StorageConfig -> ExceptT String IO ()
install cfg = do
  liftIO $ putStrLn "install"
  liftIO $ print cfg
