module Pipeline.Source.NewsAPI.Analysis where

import           Control.Lens                      ((^.))
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import           Data.Time.Clock                   (UTCTime)
import           System.FilePath                   ((</>))
--
import           NewsAPI.DB
import qualified DB.Schema.NewsAPI.Analysis        as An
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB
import           Pipeline.Type
  
getAllAnalysisFilePath :: PathConfig -> IO [(FilePath,UTCTime)]
getAllAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getAnalysisAll conn
  return $ map (\a -> (let hsh = anHshB16 a in (take 2 hsh) </> hsh,An._created a)) as

getAnalysisFilePathBySource :: PathConfig -> String -> IO [(FilePath,UTCTime)]
getAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getAnalysisBySource src conn
  return $ map (\a -> (let hsh = anHshB16 a in (take 2 hsh) </> hsh,An._created a)) as

anHshB16 :: An.AnalysisH -> FilePath
anHshB16 a = (BL8.unpack . BL8.fromStrict . B16.encode . An._sha256) a
