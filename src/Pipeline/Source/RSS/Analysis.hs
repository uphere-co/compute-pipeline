module Pipeline.Source.RSS.Analysis where

import           Control.Lens                      ((^.))
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import           System.FilePath                   ((</>))
--
import           RSS.DB
import qualified RSS.DB.Analysis            as An
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB
import           Pipeline.Type
  
getAllRSSAnalysisFilePath :: PathConfig -> IO [FilePath]
getAllRSSAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getRSSAnalysisAll conn
  return $ map (\x -> (take 2 x) </> x) $ getRSSAnalysisHashInB16 as

getRSSAnalysisFilePathBySource :: PathConfig -> String -> IO [FilePath]
getRSSAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getRSSAnalysisBySource src conn
  return $ map (\x -> (take 2 x) </> x) $ getRSSAnalysisHashInB16 as

getRSSAnalysisHashInB16 :: [An.RSSAnalysisH] -> [FilePath]
getRSSAnalysisHashInB16 as = map (BL8.unpack . BL8.fromStrict . B16.encode . An._sha256) as
