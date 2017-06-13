{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                (forM,forM_)
import qualified Data.IntMap           as IM
import           Data.List                    (foldl',nub)
import qualified Data.Map              as M
import           Data.Maybe                   (fromJust)
import qualified Data.Set              as Set
-- import qualified Data.HashMap.Strict   as HM
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Numeric.LinearAlgebra

type Vocabulary = Set.Set Text
type Doc = [Text]

-- Make vocab. Should be run for whole text from all documents.
mkVocab :: [Text] -> Vocabulary
mkVocab txts = foldl' (\acc x -> Set.insert x acc) Set.empty txts

isInVocab :: Text -> Vocabulary -> Bool
isInVocab = Set.member

isInDoc :: Text -> Doc -> Bool
isInDoc = elem

howManyInDocs :: Text -> [Doc] -> Int
howManyInDocs txt docs = length $ filter (isInDoc txt) docs

lookupVocabIndex :: Text -> Vocabulary -> Maybe Int
lookupVocabIndex = Set.lookupIndex

mkBooleanTF :: [Doc] -> Vocabulary -> [M.Map (Int,Int) Int]
mkBooleanTF docs vocab =
  let f i doc vocab = foldl' (\acc x -> M.insert (fromJust $ lookupVocabIndex x vocab,i) 1 acc) M.empty doc
  in foldl' (\acc (i,doc) -> (f i doc vocab):acc) [] (zip [1..] docs)

mkLogCountTF :: [Doc] -> Vocabulary -> [M.Map (Int,Int) Float]
mkLogCountTF docs vocab =
  let f i doc vocab = M.map (log . (+ 1)) $ foldl' (\acc x -> M.insertWith' (+) (fromJust $ lookupVocabIndex x vocab,i) 1 acc) M.empty doc
  in foldl' (\acc (i,doc) -> (f i doc vocab):acc) [] (zip [1..] docs)

main :: IO ()
main = do
  content <- TIO.readFile "/data/groups/uphere/data/filelist.txt"
  let filelist' = T.lines content
      filelist = filter (\x -> last (T.splitOn "." x) == "maintext") filelist'

  docs <- forM (take 10 filelist) $ \f -> do
    ta <- TIO.readFile $ "/home/modori/workspace/RSS.text/" ++ (T.unpack f)
    return (T.words ta)

  -- let tfc = foldl' (\acc x -> M.insertWith' (+) x 1 acc) M.empty (T.words txt)
  let vocab = mkVocab (concat docs)
  let btf = mkBooleanTF docs vocab
      ctf = mkLogCountTF docs vocab

  print btf
  print ctf
  -- print $ Set.size vocab
  -- print $ Set.size $ Set.map (\x -> (x,howManyInDocs x docs)) vocab 

  putStrLn "TF-IDF App"
