module Pipeline.Source.NewsAPI.Analysis where


import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as L8  
import           System.FilePath                   ((</>))
--
import           NewsAPI.DB
import qualified NewsAPI.DB.Analysis        as An
--
import           Pipeline.Operation.DB

  
getAllAnalysisFilePath :: IO [FilePath]
getAllAnalysisFilePath = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  analyses <- getAnalysisAll conn
  let list' = map (L8.unpack . L8.fromStrict . B16.encode . An._sha256) analyses
      list = map (\x -> (take 2 x) </> x) list'
  return list

getAnalysisFilePathBySource :: String -> IO [FilePath]
getAnalysisFilePathBySource src = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  as <- getAnalysisBySource src conn
  return $ map (\x -> (take 2 x) </> x) $ getAnalysisHashInB16 as

getAnalysisHashInB16 :: [An.AnalysisH] -> [FilePath]
getAnalysisHashInB16 as = map (L8.unpack . L8.fromStrict . B16.encode . An._sha256) as
