{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Environment            (getArgs)
--
import Statistics
import Tokenizer
import Type
import Usage
import Util
import Util.IO
import View

main :: IO ()
main = do

  sigtxt <- readSigFile sigFile
  tables <- readNETableFile tblFile

  [option] <- getArgs
 
  let sigPosWordListWithBlank    = zip [(1 :: Int)..] (sepByBlank sigtxt)
      sigOffsetWordListWithBlank = mkPostoOffset sigPosWordListWithBlank
      -- sigWordList                = T.words sigtxt
      -- sigWordWindow              = mkWordWindow sigWordList
  let offsetWordsNoBlank = filter (\ifw' -> not $ isBlank (ifw' ^. _3)) sigOffsetWordListWithBlank      

  -- numbers are essential for now.
  let sigOffsetWord2Window = mkWordWindow' 2 offsetWordsNoBlank
      annotWord2' = (\w acc -> if (isNNE' 1 0 w tables) then (w,True):acc else (w,False):acc)
      taggedWords2' = foldr annotWord2' [] sigOffsetWord2Window
  
  let sigOffsetWord3Window = mkWordWindow' 3 offsetWordsNoBlank
      annotWord3' = (\w acc -> if (isNNE' 2 0 w tables) then (w,True):acc else (w,False):acc)
      taggedWords3' = foldr annotWord3' [] sigOffsetWord3Window

  let trueTagged2 = filter (\(_,b) -> b) taggedWords2'
      trueTaggedMerged2 = map mergeNR'' trueTagged2
      trueTaggedMergedWord2 = map (\(w,_) -> w) trueTaggedMerged2 
  
  let trueTagged3 = filter (\(_,b) -> b) taggedWords3'
      trueTaggedMerged3 = map mergeNR'' trueTagged3
      trueTaggedMergedWord3 = map (\(w,_) -> w) trueTaggedMerged3 

  case option of
    "0" -> cutePrintPTW (trueTaggedMergedWord2 ++ trueTaggedMergedWord3)
    "1" -> do
      let sigOffsetWord2Window' = mkWordWindow' 3 sigOffsetWordListWithBlank
          annotWord2'' = (\w acc -> if (isNNE' 1 0 w tables) then (w,True):acc else (w,False):acc)
          taggedWords2'' = foldr annotWord2'' [] sigOffsetWord2Window'

      replaced2Text <- fmap (T.intercalate "") $ replaceTaggedWord'' taggedWords2''

      let sigPosWordListWithBlank'    = zip [(1 :: Int)..] (sepByBlank replaced2Text)
          sigOffsetWordListWithBlank' = mkPostoOffset sigPosWordListWithBlank'
          -- sigWordList'                = T.words replaced2Text
          -- sigWordWindow'              = mkWordWindow sigWordList'
      
      let sigOffsetWord3Window' = mkWordWindow' 5 sigOffsetWordListWithBlank'
          annotWord3'' = (\w acc -> if (isNNE' 2 0 w tables) then (w,True):acc else (w,False):acc)
          taggedWords3'' = foldr annotWord3'' [] sigOffsetWord3Window'

      replaced23Text <- replaceTaggedWord'' taggedWords3''
      
      TIO.writeFile "replaced.txt" (T.intercalate "" $ replaced23Text)

    _ -> error "Wrong option" 
