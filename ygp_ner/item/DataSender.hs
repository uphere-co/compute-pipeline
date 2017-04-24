{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens              ((^.),_1,_2,_3)
import           Control.Monad             (forM,forM_)
import qualified Data.Aeson          as A
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Char           as DC
import           Data.List                 (foldl',groupBy)
import qualified Data.List.Split     as DLS
-- import           Data.Text                 (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TLIO
import           System.Environment        (getArgs)
--
import           Constant
import           Tokenizer
import           Type
import           Util
import           Util.IO

main :: IO ()
main = do
  [filename] <- getArgs
  
  sigtxtRep <- fmap TL.toStrict $ TLIO.readFile filename -- "TestRow.txt"
  goodInd <- readIndicatorFile "Indicators.txt"  -- This should be taken as an argument in the future.

  let (_,_,os) = tokenizeRaw sigtxtRep
      shortlongSen = filter (\xs -> length xs > 0) os
  
  let brkLine'' line' = separateOffWordsLineBreak line'
      splitItem'' xs ys = (checkIndicator'' xs goodInd == checkIndicator'' ys goodInd)
      txtBrkLong' = map (\xs -> if ((last xs ^. _2) - (head xs ^. _1) > senLengthCut) then (brkLine'' xs) else [xs]) shortlongSen
      txtSplitBrk' = map (\xss -> if (len2 xss > senLengthCut) then (groupBy splitItem'' xss) else [xss]) txtBrkLong'
      txtSplitOnlyItem' = map (\xsss -> if (len3 xsss > senLengthCut) then (map (\xss -> if (checkIndicator'' (head xss) goodInd) then (map (\xs ->  [(-1,-1," #ITEM# ")] ++ xs ++ [(-1,-1," #ITEM# ")]) xss) else [concat xss]) xsss) else xsss) txtSplitBrk'

  let result = DLS.splitWhen (\(_,_,t) -> t == " #ITEM# ") (concat $ concat $ concat txtSplitOnlyItem')
      prettyResult' = map (\xs -> ((head xs ^. _1),(last xs ^. _2),T.intercalate "" $ map (\(_,_,t) -> t) xs)) result
      prfnt xs (_,f,t) = init xs ++ [(last xs ^. _1,f,T.append (last xs ^. _3) t)]   
      prettyResult = foldl' (\acc x@(_,_,t) -> if (T.all DC.isSpace t) then (prfnt acc x) else (acc ++ [x])) [] prettyResult'

  
  forM_ (zip [(1::Int)..] prettyResult) $ \(n,(_,_,t)) -> do
    TIO.writeFile ("data/" ++ filename ++ "." ++ (show n) ) t

  
  jsonResult <- forM (zip [(1::Int)..] prettyResult) $ \(n,(i,f,_)) -> do
    return $ OffsetData {fileName = (T.append (T.pack filename) (T.pack $ "." ++ (show n))) , begin = i, end = f}

  let json = MetaData (T.pack filename) jsonResult
  
  BL.writeFile (filename ++ ".json") (A.encode $ json)
  
  return ()
