{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run.SRL where


import           Control.Lens     ((^.),(^..),(^?),ix)
import           Data.Graph
import           Data.List        (find)
import           Data.Maybe       (catMaybes,isNothing)
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

cnvtVtxToMGV :: MeaningGraph -> Vertex -> Maybe MGVertex 
cnvtVtxToMGV mg vtx =
  let vertices = (mg ^. mg_vertices)
      mv = find (\x -> (x ^. mv_id) == vtx) vertices
  in mv
  
findLabel :: [MGVertex] -> Int -> Maybe Text
findLabel mvs i =
  let mr = find (\mv -> (mv ^. mv_id) == i) mvs
  in case mr of
    Nothing                     -> Nothing
    Just (v@(MGEntity {..}))    -> Just (v ^. mv_text)
    Just (v@(MGPredicate {..})) -> Just (v ^. mv_frame)

findAgent :: MeaningGraph -> Graph -> Vertex -> Maybe Vertex
findAgent mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> attached grph vtx ^? ix 0

findAgentTheme :: MeaningGraph -> Graph -> Vertex -> Maybe (Vertex, Vertex)
findAgentTheme mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> (,) <$> Just vtx <*> (attached grph vtx ^? ix 0)

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
          mgpredvtxs = (mgpred ^.. traverse . mv_id)
          agents = catMaybes $ map (\vtx -> findAgentTheme mg graph vtx) mgpredvtxs
          vertices = mg ^. mg_vertices
          agentsName = map (\(v1,v2) -> (,) <$> findLabel vertices v1 <*> findLabel vertices v2) agents
          reachList = map (\v -> reachable graph v) mgpredvtxs
      print agentsName
--      print $ map (\xs -> map (\x -> map (\g -> findLabel g x) (mg ^. mg_vertices)) xs) reachList
