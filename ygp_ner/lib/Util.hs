{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import           Control.Lens
import           Control.Monad                (forM_)
import           Data.Hashable
import qualified Data.Attoparsec.Text  as A
import qualified Data.Char             as DC
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import           Data.List                    (foldl')
import qualified Data.List.Split       as DLS
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Vector           as V

--
import Parser
import Type

makeFixedString :: Int -> String -> String
makeFixedString n str = str ++ (replicate (n - (length str)) (' ' :: Char))

takeSubText :: Int -> Int -> Text -> Text
takeSubText i f txt = T.take (f - i) $ T.drop i txt

mkOffset :: [Text] -> [(Int,Int,Text)]
mkOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length x),x):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkPostoOffset :: [(Int,Text)] -> [(Int,Int,Text)]
mkPostoOffset xs = reverse $ foldl' (\acc x -> ((af acc),(af acc)+(T.length (snd x)),(snd x)):acc) [(0,0,"")] xs
  where af x' = head x' ^. _2

mkSenFromOffset :: [(Int,Int,Text)] -> [[(Int,Int,Text)]]
mkSenFromOffset xs = DLS.split (DLS.whenElt (\(_,_,t) -> t == "." || t == "#ROW_BEGIN#" )) xs

mergeNR :: [(Int,Int,Text)] -> [(Int,Int,Text)]
mergeNR ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (head x ^. _1, last x ^. _2, T.append (head x ^. _3) $ T.append (T.replicate ((last x ^. _1) - (head x ^. _2)) " ") (last x ^. _3) ) ) nrl

mergeNR' :: [(Int,Text)] -> [(Int,Text)]
mergeNR' ptw = let nrl = DLS.chunksOf 2 ptw
  in map (\x -> (fst $ head x, T.append (snd $ head x) $ T.append (T.replicate ((fst $ last x) - (fst $ head x)) " ") (snd $ last x) ) ) nrl

sepBy :: Text -> Text -> [Text]
sepBy orig txt = map (T.pack) $ DLS.split (DLS.condense $ DLS.oneOf (T.unpack orig)) (T.unpack txt)

mkNumToNR :: Text -> Text
mkNumToNR txt = if (isInteger txt) then "#NUM" else txt

extractNumber :: Text -> [Text]
extractNumber = sepBy "0123456789"

replaceNumber :: Text -> Text
replaceNumber txt = T.intercalate "" $ map mkNumToNR (extractNumber txt)

isBlank' :: Text -> Bool
isBlank' = T.all (\x -> if (x == ' ') then True else False)

isBlank :: Text -> Bool
isBlank = T.all DC.isSpace

-- Put space before and after the period.
-- Remain the period.
-- Caution : This breaks the original offset.
isolatePeriod :: Text -> Text
isolatePeriod = T.replace "." " . "

-- replace period with space
changePtoS :: Zipper Text -> Zipper Text
changePtoS (Z xs y zs) = Z xs (T.replace "." " " y) zs

changePtoS' :: Zipper' (Int,Int,Text) -> Zipper' (Int,Int,Text)
changePtoS' (Z' xs ys zs) = Z' xs (map (\(i,f,t) -> (i,f,T.replace "." " " t)) ys) zs

isInteger :: Text -> Bool
isInteger txt = let suc = A.parseOnly (integer <* A.endOfInput) txt
  in case suc of
    Left  _ -> False
    Right _ -> True

isFloat :: Text -> Bool
isFloat txt = let suc = A.parseOnly (float <* A.endOfInput) txt
  in case suc of
    Left  _ -> False
    Right _ -> True

isDotted :: Text -> Bool
isDotted txt = let suc = A.parseOnly (dotted <* A.endOfInput) txt
  in case suc of
    Left  _ -> False
    Right _ -> True

isNumber :: Text -> Bool
isNumber txt = let suc = A.parseOnly (number <* A.endOfInput) txt
  in case suc of
    Left  _ -> False
    Right _ -> True

isContainingNumber :: Text -> Bool
isContainingNumber txt = let suc = A.parseOnly (containnumber <* A.endOfInput) txt
  in case suc of
    Left  _ -> False
    Right _ -> True


mkTWord :: Text -> Token
mkTWord txt = (TWord txt)

mkNumEnty :: [Text] -> NumEntity
mkNumEnty txts = map (\x -> toToken x) txts
  where toToken x = case x of
          "."       -> Period
          ","       -> Comma
          _         -> if (isInteger x) then Number else TWord x

-- Input is result of patternUsages ( = [(normalized regex-satsfied text,its word window)] )
occurList :: (Eq k, Hashable k) => [(k,v)] -> HM.HashMap k [v]
occurList = foldl' f HM.empty
 where f !acc (!k,!v) = HM.alter af k acc
          where af Nothing   = Just [v]
                af (Just vs) = Just (v:vs)

-- Input is [normalized regex-satisfied text]
occurCount :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
occurCount = foldl' f HM.empty
 where f !acc !k = HM.alter af k acc
          where af Nothing   = Just 1
                af (Just !n) = Just (n+1)

joinWith :: (Eq k, Hashable k) =>
            (Maybe v -> Maybe v' -> v'')
         -> HM.HashMap k v -> HM.HashMap k v' -> HM.HashMap k v''
joinWith f m1 m2 = foldl' fill HM.empty ks
  where fill !m !k = HM.insert k (f (HM.lookup k m1) (HM.lookup k m2)) m
        k1s = HM.keys m1 
        k2s = HM.keys m2
        ks = HS.toList (HS.fromList k1s `HS.union` HS.fromList k2s)

prettySrcPrt :: V.Vector Regulation -> IO ()
prettySrcPrt v = do
  let vs = V.toList v
      sigrec = filter (not . T.null) . map enmainrequire $ vs
  forM_ (take 20 sigrec) $ \r -> do
    print r

makeHMCounter :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
makeHMCounter = foldl' f HM.empty
  where f !acc !k = HM.alter af k acc
          where af Nothing   = Just 1
                af (Just !n) = Just (n+1)

len1 :: [(Int,Int,Text)] -> Int
len1 xs   = foldl' (\acc (_,_,t)  -> acc + (T.length t)) 0 xs

len2 :: [[(Int,Int,Text)]] -> Int
len2 xss  = foldl' (\acc xs -> acc + len1 xs) 0 xss

len3 :: [[[(Int,Int,Text)]]] -> Int
len3 xsss = foldl' (\acc xss -> acc + len2 xss) 0 xsss


lenword1 :: [(Int,Int,Text)] -> Int
lenword1 xs  = foldl' (\acc _ -> acc + (1 :: Int)) 0 xs

lenword2 :: [[(Int,Int,Text)]] -> Int
lenword2 xss = foldl' (\acc xs -> acc + lenword1 xs) 0 xss
