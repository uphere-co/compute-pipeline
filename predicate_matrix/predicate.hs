{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Set     as S
import           Data.List           (foldl')
import qualified Data.Map     as M
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import           PM.Type
import           PM.Util

main :: IO ()
main = do
  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines

  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat
      enmat100 = take 100 enmat

  let pm = createPM enmat
  print $ query (T.concat ["mcr:ili-30-","00203213-v"]) pm
  {-
  let mm = M.empty

  let dm = foldl' (\acc x -> M.insertWith' (++) (idTriple x) [x] acc) M.empty enmat100
      dm' = fmap length dm

  let dm2 = foldl' (\acc x -> M.insertWith' (++) (idQuad x) [x] acc) M.empty enmat100
      dm2' = fmap length dm2

  let dm3 = foldl' (\acc x -> M.insertWith' (++) (id4 x) [x] acc) M.empty enmat100
      dm3' = fmap length dm3

  let dm4 = foldl' (\acc x -> M.insertWith' (++) (id5 x) [x] acc) M.empty enmat100
      dm4' = fmap length dm4
  
  let dm5 = foldl' (\acc x -> M.insertWith' (++) (id6 x) [x] acc) M.empty enmat100
      dm5' = fmap length dm5

  print dm5'
  -}

  -- print $ length $ S.fromList $ map mcrIliOffset enmat
  -- print $ length $ enmat
  
  -- print dm'
  -- print $ length $ S.fromList $ map (\x-> (idPred x, idRole x, idPOS x)) enmat
  -- print $ length $ enmat

  -- print (take 10 enmat)

  

  {-
  print $ take' 50 $ S.fromList $ map idPred totalmat
  print $ take' 50 $ S.fromList $ map idRole totalmat
  print $ take' 50 $ S.fromList $ map vnClass totalmat
  print $ take' 50 $ S.fromList $ map vnClassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnSubclass totalmat
  print $ take' 50 $ S.fromList $ map vnSubclassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnLema totalmat
  print $ take' 50 $ S.fromList $ map vnRole totalmat
  print $ take' 50 $ S.fromList $ map wnSense totalmat
  print $ take' 50 $ S.fromList $ map mcrIliOffset totalmat
  print $ take' 50 $ S.fromList $ map fnFrame totalmat
  print $ take' 50 $ S.fromList $ map fnLe totalmat
  print $ take' 50 $ S.fromList $ map fnFrameElement totalmat
  print $ take' 50 $ S.fromList $ map pbRoleset totalmat
  print $ take' 50 $ S.fromList $ map pbArg totalmat
  print $ take' 50 $ S.fromList $ map mcrBC totalmat
  print $ take' 50 $ S.fromList $ map mcrDomain totalmat
  print $ take' 50 $ S.fromList $ map mcrSUMO totalmat
  print $ take' 50 $ S.fromList $ map mcrTO totalmat
  print $ take' 50 $ S.fromList $ map mcrLexname totalmat
  print $ take' 50 $ S.fromList $ map mcrBLC totalmat
  print $ take' 50 $ S.fromList $ map wnSensefrec totalmat
  print $ take' 50 $ S.fromList $ map wnSynsetRelNum totalmat
  print $ take' 50 $ S.fromList $ map esoClass totalmat
  print $ take' 50 $ S.fromList $ map esoRole totalmat
  -}
  
  {-
  PRINT $ length totalmat
  print $ length $ filter (\x -> idPOS x == "id:v") totalmat 
  print $ length $ filter (\x -> idPOS x == "id:n") totalmat
  print $ length enmat
  print $ length $ filter (\x -> idPOS x == "id:v") enmat 
  print $ length $ filter (\x -> idPOS x == "id:n") enmat
  -}
  return ()
