{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenizer where

import           Control.Lens
import           Control.Monad                (join)
import qualified Data.Char             as DC
import           Data.List                    (groupBy,null)
import qualified Data.List.Split       as DLS
import           Data.Text                    (Text)
import qualified Data.Text             as T
--
import           Util

getRows :: Text -> [Text]
getRows = T.splitOn "#ROW_BEGIN#"

sepByRow :: Text -> [Text]
sepByRow txt = map T.pack $ DLS.split (DLS.onSublist "#ROW_BEGIN#") (T.unpack txt)

sepByPeriod :: Text -> [Text]
sepByPeriod txt = map T.pack $ DLS.split (DLS.oneOf ".") (T.unpack txt)

-- Then split by period
getSen :: Text -> [Text]
getSen txt = join $ map (T.splitOn ".") $ T.splitOn "#ROW_BEGIN#" txt



-- breaks lines by line-break symbol
breakSen :: Text -> [Text]
breakSen sen = filter (\t -> (T.length t) > 0) $ sepOnLineBreak sen
-- breakSen sen = filter (\t -> (T.length t) > 0) $ foldl' (\acc x -> (separateLineBreak x) ++ acc) [] sen

checkIndicator :: Text -> [Text] -> Bool
checkIndicator txt indtbl = firstW `elem` indtbl
  where firstW = T.replace " " "" ((wordsBlank $ removeInitMargin txt) !! 0)

checkIndicator' :: [Text] -> [Text] -> Bool
checkIndicator' txts indtbl = (head txts) `elem` indtbl

checkIndicator'' :: [(Int,Int,Text)] -> [Text] -> Bool
checkIndicator'' txts indtbl =
  let txts' = dropWhile (\(_,_,t) -> T.all DC.isSpace t) txts
  in case (null txts') of
    True  -> False
    False -> (head txts ^. _3) `elem` indtbl

-- Tokenize the word separated by chars from isSpace. It is the same with the Data.Text.words, but
-- remains the blank chunks.
sepByBlank :: Text -> [Text]
sepByBlank txt = map (T.pack) $ DLS.split (DLS.condense $ DLS.whenElt DC.isSpace) (T.unpack txt)

removeInitMargin :: Text -> Text
removeInitMargin = T.dropWhile DC.isSpace

-- Removes the separator
sepOnLineBreak :: Text -> [Text]
sepOnLineBreak txt = filter (\x -> not $ (T.isInfixOf "\r" x || T.isInfixOf "\n" x)) $ sepBy "\r\n" txt

-- Leaves the separator
sepByLineBreak :: Text -> [Text]
sepByLineBreak txt = sepBy "\r\n" txt

-- Tokenize the word separated by chars from isSpace. It is the same with the Data.Text.words, but
-- remains the blank chunks.
wordsBlank :: Text -> [Text]
wordsBlank txt = map (T.pack) $ DLS.split (DLS.condense $ DLS.whenElt DC.isSpace) (T.unpack txt)


-- Tokenize the raw text. Outputs are (Int,Word), (Offset,Word), (Offset,Sentence) respectively.
-- Each of them includes the blank
type PosWord = [(Int,Text)]
type OffWord = [(Int,Int,Text)]
type OffSen  = [[(Int,Int,Text)]]
tokenizeRaw :: Text -> (PosWord,OffWord,OffSen)
tokenizeRaw txt =
  let pw = zip [(1 :: Int)..] (wordsBlank txt)
      ow = mkPostoOffset pw
      os = mkSenFromOffset ow
  in (pw,ow,os)


separateLineBreak :: Text -> [Text]
separateLineBreak txt = filter (\x -> not $ (T.isInfixOf "\r" x || T.isInfixOf "\n" x)) $ map (T.pack) $ DLS.split (DLS.condense $ DLS.oneOf "\r\n") (T.unpack txt)

separateWordsLineBreak :: [Text] -> [[Text]]
separateWordsLineBreak txts = groupBy lbk txts
  where lbk x y = (isBrk x == isBrk y)
        isBrk x = (T.isInfixOf "\r" x || T.isInfixOf "\n" x)

separateOffWordsLineBreak :: [(Int,Int,Text)] -> [[(Int,Int,Text)]]
separateOffWordsLineBreak txts = groupBy lbk txts
  where lbk x y       = (isBrk x == isBrk y)
        isBrk (_,_,t) = (T.isInfixOf "\r" t || T.isInfixOf "\n" t)
