{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Data.Function              (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L   (groupBy,lookup)
import           Data.List.Split            (chunksOf)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
-- import qualified Data.Text.Lazy      as TL
-- import qualified Data.Text.Lazy.IO   as TLIO
import           System.IO

main' = do
  let fp = "/scratch/groups/uphere/wikidata/ne.person"
  txt <- TIO.readFile fp
  let txts = T.lines txt
      m = HM.fromList . map (\(x:xs)-> (x,T.intercalate " " xs)) . map T.words $ txts
  print (HM.lookup "Q354" m)

main = do
  let fp = "/scratch/groups/uphere/wikidata/wikidata.all_entities"
  txt <- TIO.readFile fp
  let txts = T.lines txt
      n = length txts
      neach = (n `div` 20) + 1
      txtss = chunksOf neach txts

  ref <- atomically $ newTVar []
  flip mapM_ (zip [0..] txtss) $ \(n,txts) ->
    forkIO $ do
      let xs = HM.fromList
             . map (\xs -> (fst (head xs), map snd xs)) 
             . L.groupBy ((==) `on` fst)
             . map (\(x:xs) -> (x, T.intercalate " " xs))
             . map T.words
             $ txts
      deepseq xs $ do
        -- (print . HM.size) xs
        atomically $ do
          array <- readTVar ref
          writeTVar ref ((n,xs):array)

  xss <- atomically $ do
           array <- readTVar ref
           if (length array /= 20) then retry else return (map snd array)
  (print . concat . mapMaybe (HM.lookup "Q349")) xss

