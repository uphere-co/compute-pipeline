{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.IntMap           as IM
import           Data.List                    (foldl')
import qualified Data.Map              as M
import qualified Data.Set              as Set
-- import qualified Data.HashMap.Strict   as HM
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Numeric.LinearAlgebra

testM1 = [((10000,10000),1.5),((9999,9999),0.2)]

-- Make vocab. Should be run for whole text from all documents.
mkVocab txts = foldl' (\acc x -> Set.insert x acc) Set.empty txts

isInVocab :: Text -> Set.Set Text -> Bool
isInVocab = Set.member

lookupVocabIndex txt vocab = Set.lookupIndex txt vocab


main :: IO ()
main = do
  txt <- TIO.readFile "test.txt"
  txt2 <- TIO.readFile "test2.txt"
  print txt
  let result = foldl' (\acc x -> M.insertWith' (+) x 1 acc) M.empty (T.words txt)
  let vocab = mkVocab ((T.words txt) ++ (T.words txt2))
  let docindex = 1
  let tf = foldl' (\acc x -> ((lookupVocabIndex x vocab,docindex),1):acc) [] (T.words txt)


  -- print $ (mkSparse testM1)
  print result
  print vocab
  print tf
  print $ Set.size vocab
  putStrLn "TF-IDF App"
