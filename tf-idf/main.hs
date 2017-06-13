{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                (forM,forM_)
import qualified Data.IntMap           as IM
import           Data.List                    (foldl')
import qualified Data.Map              as M
import           Data.Maybe                   (fromJust)
import qualified Data.Set              as Set
-- import qualified Data.HashMap.Strict   as HM
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Numeric.LinearAlgebra

type Vocabulary = Set.Set Text

-- Make vocab. Should be run for whole text from all documents.
mkVocab :: [Text] -> Vocabulary
mkVocab txts = foldl' (\acc x -> Set.insert x acc) Set.empty txts

isInVocab :: Text -> Vocabulary -> Bool
isInVocab = Set.member

lookupVocabIndex :: Text -> Vocabulary -> Maybe Int
lookupVocabIndex = Set.lookupIndex

main :: IO ()
main = do

  content <- TIO.readFile "/data/groups/uphere/data/filelist.txt"
  let filelist' = T.lines content
      filelist = filter (\x -> last (T.splitOn "." x) == "maintext") filelist'

  txts' <- forM (take 3 filelist) $ \f -> do
    ta <- TIO.readFile $ "/home/modori/workspace/RSS.text/" ++ (T.unpack f)
    return ta

  let txts = concat $ map T.words txts'

  -- let tfc = foldl' (\acc x -> M.insertWith' (+) x 1 acc) M.empty (T.words txt)
  let vocab = mkVocab txts

  tfs <- forM (take 1 (zip [1..] filelist)) $ \(i,f) -> do
    ta <- TIO.readFile $ "/home/modori/workspace/RSS.text/" ++ (T.unpack f)
    let tf = foldl' (\acc x -> ((fromJust $ lookupVocabIndex x vocab,i),1):acc) [] (T.words ta)
    return tf
    
  print $ Set.size vocab

  putStrLn "TF-IDF App"
