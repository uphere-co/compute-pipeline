{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run.SRL where

import           Control.Lens     ((^.),(^..),(^?),_1,_2,_3,ix)
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

cnvtEdgToMGE :: MeaningGraph -> Edge -> Maybe MGEdge
cnvtEdgToMGE mg edg =
  let edges = (mg ^. mg_edges)
      me = find (\x -> (x ^. me_start) == (fst edg) && (x ^. me_end) == (snd edg)) edges
  in me

findRel :: [MGEdge] -> Int -> Int -> Maybe Text
findRel mes i j =
  let mr = find (\me -> (me ^. me_start) == i && (me ^. me_end) == j) mes
  in case mr of
    Nothing -> Nothing
    Just e  -> Just (e ^. me_relation)

findLabel :: [MGVertex] -> Int -> Maybe Text
findLabel mvs i =
  let mr = find (\mv -> (mv ^. mv_id) == i) mvs
  in case mr of
    Nothing                     -> Nothing
    Just (v@(MGEntity {..}))    -> Just (v ^. mv_text)
    Just (v@(MGPredicate {..})) -> Just (v ^. mv_frame)

findAgent :: MeaningGraph -> Graph -> Vertex -> Maybe Edge
findAgent mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   agent' = find (\(t,i,j) -> t == "Agent") rels
                   agent = fmap (\x -> (x ^. _2, x ^. _3)) agent'
               in agent

type ARB = (Vertex, Vertex, Vertex)

findAgentTheme :: MeaningGraph -> Graph -> Vertex -> Maybe ARB
findAgentTheme mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   agent = fmap (^. _3) $ find (\(t,i,j) -> t == "Agent") rels
                   theme = fmap (^. _3) $ find (\(t,i,j) -> t == "Theme") rels
               in ((,,) <$> agent <*> Just vtx <*> theme)

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
          agentsName = map (\(v1,v2,v3) -> (,,) <$> findLabel vertices v1 <*> findLabel vertices v2 <*> findLabel vertices v3) agents
      case agentsName of
        [] -> return ()
        an -> print an
