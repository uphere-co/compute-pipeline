{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Storage.Operation where

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (ExceptT)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.UUID.V4                      (nextRandom)
import           System.Directory.Tree             (DirTree(..),dirTree,flattenDir,readDirectory)
import           System.FilePath                   ((</>))
--
import           Storage.Config                  (StorageConfig)

mkFileList :: FilePath -> DirTree a -> [FilePath]
mkFileList fp (Dir n cs)    = concatMap (mkFileList (fp </> n)) cs
mkFileList fp (File n _)    = [fp </> n]
mkFileList fp (Failed _  _) = []

mkManifest :: DirTree a -> Text
mkManifest d = T.unlines (map T.pack (mkFileList "" d))


register :: StorageConfig -> FilePath -> ExceptT String IO ()
register cfg fp = do
  liftIO $ putStrLn "register"
  liftIO $ do i <- nextRandom
              print i
  liftIO $ print cfg
  dir <- liftIO $ readDirectory fp
  -- liftIO $ mapM_ print (mkFileList "" (dirTree dir))
  liftIO $ TIO.putStrLn (mkManifest (dirTree dir))

install :: StorageConfig -> ExceptT String IO ()
install cfg = do
  liftIO $ putStrLn "install"
  liftIO $ print cfg

