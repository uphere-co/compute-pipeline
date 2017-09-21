module Pipeline.Source.NewsAPI.Analysis where

import           Control.Lens                      ((^.))
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import           System.FilePath                   ((</>))
--
import           NewsAPI.DB
import qualified NewsAPI.DB.Analysis        as An
--
import           Pipeline.Operation.DB
import           Pipeline.Type
  
getAllAnalysisFilePath :: PathConfig -> IO [FilePath]
getAllAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getAnalysisAll conn
  return $ map (\x -> (take 2 x) </> x) $ getAnalysisHashInB16 as

getAnalysisFilePathBySource :: PathConfig -> String -> IO [FilePath]
getAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getAnalysisBySource src conn
  return $ map (\x -> (take 2 x) </> x) $ getAnalysisHashInB16 as

getAnalysisHashInB16 :: [An.AnalysisH] -> [FilePath]
getAnalysisHashInB16 as = map (BL8.unpack . BL8.fromStrict . B16.encode . An._sha256) as
