{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Statistics where

import           Control.Monad.Trans.State    (execStateT)

import           Control.Lens                 (over)
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (forM,forM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.State.Class    (modify')
import           Data.Char                    (isSpace)
import           Data.Function                (on)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (foldl',sortBy)
import           Data.Text                    (Text)
import qualified Data.Text              as T
--
import           Type
import           Usage
import           Util


data TagTable = TagTable { _counter :: HM.HashMap Text Int }
  deriving (Show)

makeLenses ''TagTable

-- emptyTagTable :: TagTable
-- emptyTagTable = TagTable { _counter = HM.empty }

-- Pattern Usages (PU)
-- Input is HashMap of (pattern, [matched word windows])
-- output is sorted [pattern, number of matched pattern].
prettyPrintPU :: HM.HashMap Text [Zipper Text] -> IO ()
prettyPrintPU hm = forM_ (sortBy (flip compare `on` snd) $ HM.toList $ HM.map length hm) $ \(x,y) -> do
  liftIO $ print ((T.unpack x) ++ "  " ++ (show y))

-- Check if the given word window is Numeric Named Entity(NNE).
isNNE :: Zipper Text -> NETTagger -> Bool
isNNE w tables =
  let p1   = filter (\x -> length x == 1) $ map fst tables
      cur  = current w
      mprv = prev w
  in case mprv of
    Nothing  -> False
    Just prv -> ((isContainingNumber cur) && ((current prv) `elem` (concat p1)))

-- LEFT_n .. NUM .. RIGHT_m. Currently, set m=0.
isNNE' :: Int -> Int -> Zipper' (Int,Int,Text) -> NETTagger -> Bool
isNNE' n m w tables =
  let n'  = n + m
      pn' = filter (\x -> length x == n') $ map fst tables
      cur = filter (\(_,_,t) -> not $ T.all isSpace t) $ current' w
  in if (length cur == n' + 1) then (matchWithNETagger cur pn') else False
  -- ((isContainingNumber (last cur)) && (head cur `elem` (concat pn')))

-- Only for m=0
matchWithNETagger :: [(Int,Int,Text)] -> [[Text]] -> Bool
matchWithNETagger c p =
  let table = map (T.intercalate " ") p
      word  = T.intercalate " " (init (map (\(_,_,z) -> z) c))
  in isContainingNumber ((\(_,_,z) -> z) $ last c) && word `elem` table

countTaggedWords :: [Zipper Text] -> NETTagger -> IO ()
countTaggedWords ww tables = do
  let p1  = filter (\x -> length x == 1) $ map fst tables
      -- p2  = filter (\x -> length x == 2) $ map fst tables
      f !acc !k = HM.insert k (0 :: Int) acc
      ss  = foldl' f HM.empty (concat p1)
  let tag = TagTable { _counter = ss }
  
  ct <- flip execStateT tag $ forM ww $ \w -> do
    let cur = current w
        mprv = prev w
    case mprv of
      Nothing  -> return ()
      Just prv -> if ((isContainingNumber cur) && ((current prv) `elem` (concat p1)))
        then modify' $ over counter (HM.adjust (\x -> x + 1) (current prv))
        else return ()

  print ct

-- Sentence distribution w.r.t. length
getSenLenDist :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
getSenLenDist = foldl' f HM.empty
  where f !acc !k = HM.alter af k acc
          where af Nothing   = Just 1
                af (Just !n) = Just (n+1)
