{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens              ((^.),_1,_2)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (groupBy)
import qualified Data.List.Split     as DLS
-- import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
--
import           Tokenizer
import           Util
import           Util.IO

main :: IO ()
main = do
  sigtxtRep <- fmap TL.toStrict $ TLIO.readFile "RepData.txt"
  goodInd <- readIndicatorFile "Indicators.txt"  -- This should be taken as an argument in the future.

  let (_,_,os) = tokenizeRaw sigtxtRep
      shortlongSen = filter (\xs -> length xs > 0) os

  let senLengthCut = 0
  let brkLine'' line' = separateOffWordsLineBreak line'
      splitItem'' xs ys = (checkIndicator'' xs goodInd == checkIndicator'' ys goodInd)
      txtBrkLong' = map (\xs -> if ((last xs ^. _2) - (head xs ^. _1) > senLengthCut) then (brkLine'' xs) else [xs]) shortlongSen
      txtSplitBrk' = map (\xss -> if (len2 xss > senLengthCut) then (groupBy splitItem'' xss) else [xss]) txtBrkLong'
      txtSplitOnlyItem' = map (\xsss -> if (len3 xsss > senLengthCut) then (map (\xss -> if (checkIndicator'' (head xss) goodInd) then (map (\xs ->  [(-1,-1," #ITEM# ")] ++ xs ++ [(-1,-1," #ITEM# ")]) xss) else [concat xss]) xsss) else xsss) txtSplitBrk'

  let result = DLS.splitWhen (\(_,_,t) -> t == " #ITEM# ") (concat $ concat $ concat txtSplitOnlyItem')

  -- TIO.writeFile "AnnotatedSentences.txt" (T.pack $ show result)
  -- print $ makeHMCounter $ map (\xs -> lenword1 xs) result
  TIO.writeFile "AnnotatedSentences.txt" $ (T.pack $ show (HM.toList $ makeHMCounter $ map (\xs -> lenword1 xs) result))
  -- cutePrint $ take 500 $ map (\xs -> T.intercalate "" (map (\(i,f,t) -> t) xs)) result
  return ()
