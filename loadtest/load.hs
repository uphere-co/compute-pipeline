{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L   (lookup)
import           Data.List.Split            (chunksOf)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.IO

main' = do
  let fp = "/scratch/groups/uphere/wikidata/ne.person"
  txt <- TIO.readFile fp
  let txts = T.lines txt
      m = HM.fromList . map (\(x:xs)-> (x,T.intercalate " " xs)) . map T.words $ txts
  -- print (HM.size m)
  print (HM.lookup "Q354" m)

main = do
  let fp = "/scratch/groups/uphere/wikidata/wikidata.all_entities"
  txt <- TIO.readFile fp
  let txts = T.lines txt
      n = length txts
      neach = (n `div` 20) + 1
      txtss = chunksOf neach txts

  ref <- atomically $ newTVar [] -- (zip [0..] (replicate 20 Nothing))
  
  flip mapM_ (zip [0..] txtss) $ \(n,txts) -> -- forkIO $ do
    deepseq txts $ forkIO $ do
      let xs = HM.fromList . map (\(x:xs) -> (x,T.intercalate " " xs)) . map T.words $ txts
      deepseq xs $ do
        (print . HM.size) xs
        atomically $ do
          array <- readTVar ref
          writeTVar ref ((n,xs):array)

  xss <- atomically $ do
           array <- readTVar ref
           if (length array /= 20) then retry else return (map snd array)
          
  let m = HM.unions xss
  print (HM.lookup "Q349" m) -- (HM.size m)
  return ()
  -- mapM_ ( ms
  -- print (HM.size m)

  -- print ()


  
{-   withFile fp ReadMode $ \h -> do
     let sz = hFileSize h
        szeach = sz `div` 20
        ptrs = map (*szeach) [0..19]
   -}     
    
        
    {- 
    txt <- TIO.readFile fp
    let txts = T.lines txt
        m = HM.fromList . map (\(x:xs)-> (x,T.intercalate " " xs)) . map T.words $ txts
        -- print (HM.size m)
    print (HM.lookup "Q354" m)
    -}
