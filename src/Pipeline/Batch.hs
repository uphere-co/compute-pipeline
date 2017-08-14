module Pipeline.Batch where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Pipeline.Source.NewsAPI.Article

data BatchSource = NYT | NewsAPI

-- runBatch NYT = runNYTbatch
runBatch NewsAPI = runNewsAPIbatch

-- runNYTbatch = return ()

runNewsAPIbatch action = do
  articles' <- getTimeTitleDescFromSrc "bloomberg"
  mapM action (map (\(_,_,x) -> x) $ (take 10 $ catMaybes articles') :: [Text])
  
