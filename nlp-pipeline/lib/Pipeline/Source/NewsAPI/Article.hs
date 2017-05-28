{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.NewsAPI.Article where

import qualified Data.ByteString.Char8      as B
import           Data.Aeson                      (eitherDecodeStrict)
import           Data.Foldable                   (toList)
import           Data.List                       (sort)
import           Data.Text                       (Text)
import           System.Directory.Tree           (dirTree,readDirectoryWith)
--
import           Intrinio.Type

getDescription :: FilePath -> IO Text
getDescription f = do
    bstr <- B.readFile f
    let ea = eitherDecodeStrict bstr :: Either String SourceArticles
    case ea of
      Left  _ -> return ""
      Right a -> return (maybe "" id (_description a))

getFileList :: FilePath -> IO ([FilePath])
getFileList fp = do
  list' <- readDirectoryWith return fp
  let filelist = sort . toList $ dirTree list'
  return filelist
