{-# LANGUAGE RecordWildCards  #-}

module Pipeline.Run.SRL where


import           Control.Lens
import           Data.Graph
import           Data.List        (find)
import           Data.Text        (Text)
import qualified Data.Text as T
import qualified Data.Tree as Tr
--
import           SRL.Analyze.Type
import           SRL.Statistics

isMGPredicate :: MGVertex -> Bool
isMGPredicate (MGEntity {..}) = False
isMGPredicate (MGPredicate {..}) = True

isMGEntity :: MGVertex -> Bool
isMGEntity = (not . isMGPredicate)

findLabel :: MGVertex -> Int -> Maybe Text
findLabel mv i =
  if ((mv ^. mv_id) == i)
  then case mv of
    v@(MGEntity {..})    -> Just (v ^. mv_text)
    v@(MGPredicate {..}) -> Just (v ^. mv_frame)
  else Nothing

findAgent :: MGVertex -> Maybe MGVertex
findAgent mv = case (isMGPredicate mv) of
  False -> Nothing
  True  -> Nothing

attached :: Graph -> Vertex -> [Vertex]
attached grph vtx =
  let lnodes = concat $ fmap Tr.levels $ dfs grph [vtx]
      mlnode = drop 1 lnodes
  in case mlnode of
    []    -> []
    lnode -> head lnode

mkARB mg = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing    -> print ""
    Just graph -> do
      let mgpred = filter isMGPredicate (mg ^. mg_vertices)
          reachList = map (\v -> reachable graph v) (mgpred ^.. traverse . mv_id)
      print $ map (\xs -> map (\x -> map (\g -> findLabel g x) (mg ^. mg_vertices)) xs) reachList
