{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Char           as DC
import qualified Data.HashMap.Strict as HM
import           Data.List                 (foldl',partition,sort,union)
-- import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO   as TIO
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
--
import           Constant
import           Tokenizer
import           Util

main :: IO ()
main = do
  sigtxtRep <- fmap TL.toStrict $ TLIO.readFile "RepData.txt"

  -- Collecting indicators and its counter
  let shortlongSen = partition (\x -> (T.length x) > senLengthCut) (getSen sigtxtRep)
      longSen = fst shortlongSen
      lines' = filter (\x -> (T.length x) > 0) $ foldl' (\acc x -> (separateLineBreak x) ++ acc) [] longSen
      indicators = filter (\x -> (T.length x) > 0) $
        foldl' (\acc x -> (T.replace " " "" ((wordsBlank $ removeInitMargin x) !! 0)):acc) [] lines'
      indCounter = makeHMCounter indicators
      indCounterCut = HM.filter (\x -> x > indOccerrenceCut) indCounter
      indCounterCutList = sort $ HM.toList indCounterCut

  -- Getting Good indicators
  let aboveCutIndicators = map (\(x,_) -> x) indCounterCutList
      aboveCutIndicatorsWithNoLetter = filter (\x -> not $ T.all DC.isLetter x) $ map (\(x,_) -> x) indCounterCutList    
      indList = map (\(x,_) -> x) $ sort $ HM.toList indCounter
      similar x y = if (T.head x == T.head y && T.last x == T.last y) then True else False
      similar' x ys = any (similar x) ys
      indicatorsBySimilarity = filter (\x -> similar' x aboveCutIndicatorsWithNoLetter) indList
      goodInd = aboveCutIndicators `union` indicatorsBySimilarity

  TIO.writeFile "Indicators.txt" (T.pack (show goodInd))
