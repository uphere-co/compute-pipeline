{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run.SRL where

import           Control.Lens     ((^.),(^..),_2,_3)
import           Data.Graph
import           Data.List        (find)
import           Data.Maybe       (catMaybes,fromJust,isNothing)
import           Data.Text        (Text)
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
  let vrtcs = (mg ^. mg_vertices)
      mv = find (\x -> (x ^. mv_id) == vtx) vrtcs
  in mv

cnvtEdgToMGE :: MeaningGraph -> Edge -> Maybe MGEdge
cnvtEdgToMGE mg edg =
  let edgs = (mg ^. mg_edges)
      me = find (\x -> (x ^. me_start) == (fst edg) && (x ^. me_end) == (snd edg)) edgs
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
                   agent' = find (\(t,_i,_j) -> t == "Agent") rels
                   agent = fmap (\x -> (x ^. _2, x ^. _3)) agent'
               in agent

type ARB = (Vertex, Vertex, [Vertex])

findAgentThemes :: MeaningGraph -> Graph -> Vertex -> Maybe ARB
findAgentThemes mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   agent = fmap (^. _3) $ find (\(t,_i,_j) -> t == "Agent") rels
                   theme1 = fmap (^. _3) $ find (\(t,_i,_j) -> t == "Theme") rels
               in ((,,) <$> agent <*> Just vtx <*> (sequence [theme1]))

isSubject t = t == "Agent" || t == "Speaker"

findSubjectObjects :: MeaningGraph -> Graph -> Vertex -> Maybe ARB
findSubjectObjects mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   subject = fmap (^. _3) $ find (\(t,_i,_j) -> isSubject t) rels
                   objects = map (\x -> Just (x ^. _3)) $ filter (\(t,i,j) -> if (isNothing $ cnvtVtxToMGV mg j) then False else (if (isMGEntity $ fromJust $ cnvtVtxToMGV mg j) then True else False)) rels
               in ((,,) <$> subject <*> Just vtx <*> (sequence objects))

                  
attached :: Graph -> Vertex -> [Vertex]
attached grph vtx =
  let lnodes = concat $ fmap Tr.levels $ dfs grph [vtx]
      mlnode = drop 1 lnodes
  in case mlnode of
    []    -> []
    lnode -> head lnode

mkARB :: MeaningGraph -> IO ()
mkARB mg = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing    -> print ("" :: String)
    Just graph -> do
      let mgpred = filter isMGPredicate (mg ^. mg_vertices)
          mgpredvtxs = (mgpred ^.. traverse . mv_id)
          agents = catMaybes $ map (\vtx -> findSubjectObjects mg graph vtx) mgpredvtxs -- (\vtx -> findAgentThemes mg graph vtx) mgpredvtxs
          vrtcs = mg ^. mg_vertices
          agentsName = map (\(v1,v2,vs) -> (,,) <$> findLabel vrtcs v1 <*> findLabel vrtcs v2 <*> (sequence $ map (\v3 -> findLabel vrtcs v3) vs)) agents
      case agentsName of
        [] -> return ()
        an -> print an
