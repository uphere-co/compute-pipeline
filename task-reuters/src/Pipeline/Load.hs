{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Load where

import           Control.Exception              (SomeException,try)
import           Control.Monad                  (forM)
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Foldable                  (toList)
import           Data.List                      (sort)
import           Data.Text                      (Text)
import           Data.Time.Clock                (UTCTime)
import           System.Directory.Tree
--
import           NLP.Shared.Type                (PathConfig)
import           SRL.Analyze.Type
import           WikiEL.Type                    (EntityMention)
--

loadCoreNLPResult :: [(FilePath,UTCTime)]
                  -> IO [Maybe (FilePath,UTCTime,Maybe DocAnalysisInput)]
loadCoreNLPResult fptms = do
  forM fptms $ \(fp,tm) -> do
    ebstr <- try $ B.readFile fp
    case ebstr of
      Left  (_e :: SomeException) -> return Nothing
      Right bstr -> return $ Just (fp,tm,A.decode (BL.fromStrict bstr))


loadWikiELResult :: [FilePath] -> IO [(FilePath,Maybe [EntityMention Text])]
loadWikiELResult fps = do
  forM fps $ \fp -> do
    bstr <- B.readFile fp
    return $ (fp,A.decode (BL.fromStrict bstr))

getFileListRecursively :: FilePath -> IO [FilePath]
getFileListRecursively fp = do
  list' <- readDirectoryWith return fp
  let filelist = sort . toList $ dirTree list'
  return filelist

loadConfigFile :: FilePath -> IO (PathConfig)
loadConfigFile fp = do
  epc <- A.eitherDecode' <$> BL.readFile fp
  case epc of
    Left e -> error e
    Right pc -> return pc
