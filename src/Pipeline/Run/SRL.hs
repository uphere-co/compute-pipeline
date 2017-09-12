{-# LANGUAGE RecordWildCards  #-}

module Pipeline.Run.SRL where


import           Control.Lens
import           Data.Graph
import           Data.List        (find)
import           Data.Text        (Text)
import qualified Data.Text as T
--
import           SRL.Analyze.Type
import           SRL.Statistics

isMGPredicate :: MGVertex -> Bool
isMGPredicate (MGEntity {..}) = False
isMGPredicate (MGPredicate {..}) = True

isMGEntity :: MGVertex -> Bool
isMGEntity = (not . isMGPredicate)

findLabel :: [MGVertex] -> Int -> Maybe Text
findLabel mgvs i =
  let mr = find (\t -> (t ^. mv_id) == i) mgvs
  in case mr of
    Nothing -> Nothing
    Just v@(MGEntity {..})    -> Just (v ^. mv_text)
    Just v@(MGPredicate {..}) -> Just (v ^. mv_frame)
    
mkARB mg = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing    -> print ""
    Just graph -> do
      let mgpred = filter isMGPredicate (mg ^. mg_vertices)
          reachList = map (\v -> reachable graph v) (mgpred ^.. traverse . mv_id)
      print $ map (\xs -> map (\x -> findLabel (mg ^. mg_vertices) x) xs) reachList
