module Pipeline.Batch where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Pipeline.Source.NewsAPI.Article

data BatchSource = NYT | NewsAPI

-- runBatch NYT = runNYTbatch
runBatch NewsAPI = runNewsAPIbatch

-- runNYTbatch = return ()

runNewsAPIbatch action = do
  articles' <- (fmap (take 10) $ getTimeTitleDescFromSrcWithHash "bloomberg")
  flip mapM (catMaybes articles') $ \(hsh,_,_,x) -> do
    result <- action x
    return (hsh,result)
  
