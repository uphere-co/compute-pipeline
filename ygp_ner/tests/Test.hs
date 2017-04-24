{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad                      (forever,forM_)
import           Control.Monad.Trans                (liftIO)
import           Control.Monad.Trans.Resource       (runResourceT)
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Conduit.Binary          as CB
import           Data.Maybe                         (fromJust)
import           System.IO

--
import           Statistics
import           Type
import           Usage
import           Util.IO

main :: IO ()
main = do

  
  -- putStrLn "Hello, Test!"

  txt <- hGetContents stdin
  hPutStrLn stdout txt
  
  -- sigtxt <- readSigFile sigFile
  -- bkgtxt <- readBkgFile bkgFile


  -- printZipper1 testZipperData1
  
  -- Print UID list for pattern only.
  -- cutePrintPTW mNR

  -- Print UID list for item, pattern and offset
  -- cutePrintNER mNR

  -- Save the original text with replacement '.' to ' ' in Numeric Reference.
  -- TIO.writeFile "result.txt" (T.intercalate "" (take 2000 (map (\x -> snd x) posTaggedReplacedWords)))

  -- Cute printing for debugging
  {-
  let chunkedTaggedWords = take 2000 $ DLS.chunksOf 24 posTaggedReplacedWords
      
  forM_ chunkedTaggedWords $ \w -> do
    print $ T.intercalate "" (map (\(x,y) -> y) w)
    -- cutePrintAnnot (AnnotText w)
    -- cutePrintOnlyAnnot (AnnotText w)
  -}
  {-
  let ww = mkWordWindow' 3 sigWordList
  forM_ ww $ \w -> do
    if (isNNE' 2 0 w tables) then (print $ current' w) else return ()
  -}




-- Test Functions
printZipper1 :: Zipper' Int -> IO ()
printZipper1 z = do
  let n1mkz' = fromJust $ next' z
      n2mkz' = fromJust $ next' n1mkz'
      n3mkz' = fromJust $ next' n2mkz'
      n4mkz' = fromJust $ next' n3mkz'
      n5mkz' = fromJust $ next' n4mkz'
      n6mkz' = fromJust $ next' n5mkz'
      n7mkz' = fromJust $ next' n6mkz'
      n8mkz' = fromJust $ next' n7mkz'

      m7mkz' = fromJust $ prev' n8mkz'
      m6mkz' = fromJust $ prev' m7mkz'
      m5mkz' = fromJust $ prev' m6mkz'
      m4mkz' = fromJust $ prev' m5mkz'
      m3mkz' = fromJust $ prev' m4mkz'
      m2mkz' = fromJust $ prev' m3mkz'
      m1mkz' = fromJust $ prev' m2mkz'
      m0mkz' = fromJust $ prev' m1mkz'
                
  print $ z
  print $ n1mkz'
  print $ n2mkz'
  print $ n3mkz'
  print $ n4mkz'
  print $ n5mkz'
  print $ n6mkz'
  print $ n7mkz'
  print $ n8mkz'

  print $ m7mkz'
  print $ m6mkz'
  print $ m5mkz'
  print $ m4mkz'
  print $ m3mkz'
  print $ m2mkz'
  print $ m1mkz'
  print $ m0mkz'
  


-- Test data

testZipperData1 :: Zipper' Int
testZipperData1 = mkZipper' 2 [1,2,3,4,5,6,7,8,9,10]
