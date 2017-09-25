module Pipeline.Source.RSS.Analysis where

import           Control.Lens                      ((^.))
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import           Data.Time.Clock                   (UTCTime)
import           System.FilePath                   ((</>))
--
import           RSS.DB
import qualified RSS.DB.Analysis            as RAn
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB
import           Pipeline.Type

getAllRSSAnalysisFilePath :: PathConfig -> IO [(FilePath,UTCTime)]
getAllRSSAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getRSSAnalysisAll conn
  return $ map (\a -> (let hsh = ranHshB16 a in (take 2 hsh) </> hsh,RAn._created a)) as

getRSSAnalysisFilePathBySource :: PathConfig -> String -> IO [(FilePath,UTCTime)]
getRSSAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- getRSSAnalysisBySource src conn
  return $ map (\a -> (let hsh = ranHshB16 a in (take 2 hsh) </> hsh,RAn._created a)) as

ranHshB16 :: RAn.RSSAnalysisH -> FilePath
ranHshB16 a = (BL8.unpack . BL8.fromStrict . B16.encode . RAn._sha256) a
