{-# LANGUAGE Arrows              #-}

module Pipeline.Source.RSS.Analysis where

import           Control.Arrow
import           Control.Lens                      ((^.))
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import qualified Data.Text                  as T
import           Data.Time.Clock                   (UTCTime)
-- import           Opaleye                           (runQuery)
import           Opaleye                    hiding (constant)
import           System.FilePath                   ((</>),takeBaseName)
--
-- import           RSS.DB
import           DB.Operation                      (queryRSSAnalysisAll,queryRSSAnalysisBySource)
import qualified DB.Schema.RSS.Analysis     as An
import qualified DB.Schema.RSS.Analysis     as RAn
import           Model.Opaleye.ShowConstant        (constant)
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB             (getConnection)
import           Pipeline.Type


getAllRSSAnalysisFilePath :: PathConfig -> IO [(FilePath,UTCTime)]
getAllRSSAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runQuery conn queryRSSAnalysisAll  -- getRSSAnalysisAll conn
  return $ map (\a -> (let hsh = ranHshB16 a in (take 2 hsh) </> hsh,RAn._created a)) as


getRSSAnalysisFilePathBySource :: PathConfig -> String -> IO [(FilePath,UTCTime)]
getRSSAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runQuery conn (queryRSSAnalysisBySource src) -- getRSSAnalysisBySource conn src
  return  $ map (\a -> (let hsh = ranHshB16 a in (take 2 hsh) </> hsh,RAn._created a)) as

-- (filter (\(h,_) -> takeBaseName h == "8b638633ec8ead0aeae84bff9dce786ccaee1bd0d22ad940dde532d8e86d922c"))

getNewItemsForSRL :: PathConfig -> String -> IO [(FilePath,UTCTime)]
getNewItemsForSRL cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runQuery conn $ proc () -> do
    r <- An.queryAll -< ()
    restrict -< (An._source r .== constant (T.pack src)) .&& (An._corenlp r .== toNullable (constant True)) .&& (isNull (An._srl r)  .|| (An._srl r .== toNullable (constant False)))
    returnA -< r
  return  $ map (\a -> (let hsh = ranHshB16 a in (take 2 hsh) </> hsh,RAn._created a)) as



ranHshB16 :: RAn.RSSAnalysisH -> FilePath
ranHshB16 a = (BL8.unpack . BL8.fromStrict . B16.encode . RAn._hash) a
