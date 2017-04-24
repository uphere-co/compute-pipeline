{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Usage where

import           Control.Lens
import           Control.Monad               (forM,forM_,join)
import           Control.Monad.State
import           Data.Function               (on)
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe                  (catMaybes,isNothing)
import           Data.Monoid                 ((<>))
import           Data.List                   (any,foldl',sortBy)
import           Data.Text                   (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
--
import           Pattern
import           Util
import           Type

mkZipper :: [a] -> Zipper a
mkZipper []     = error "Zipper should be applied to non-empty list."
mkZipper (x:xs) = Z [] x xs

current :: Zipper a -> a
current (Z _ x _) = x

next :: Zipper a -> Maybe (Zipper a)
next (Z xs y (z:zs)) = Just (Z (y:xs) z zs)
next (Z _ _ [])      = Nothing

prev :: Zipper a -> Maybe (Zipper a)
prev (Z (x:xs) y zs) = Just (Z xs x (y:zs))
prev (Z [] _ _)     = Nothing

inRange :: Int -> Int -> Zipper a -> Zipper a
inRange b a (Z xs y zs) = Z (take b xs) y (take a zs) 

mkZipper' :: Int -> [a] -> Zipper' a
mkZipper' _ []      = error "Zipper should be applied to non-empty list."
mkZipper' n xs
  | n > (length xs) = error "Text length should not be shorter than word window size."
  | otherwise       = Z' [] (take n xs) (drop n xs) 

getBefore' :: Zipper' a -> [a]
getBefore' (Z' xs _ _) = reverse xs

getAfter' :: Zipper' a -> [a]
getAfter' (Z' _ _ zs) = zs

umkZipper' :: Zipper' a -> [a]
umkZipper' (Z' xs ys zs) = (reverse xs) ++ ys ++ zs 

current' :: Zipper' a -> [a]
current' (Z' _ xs _) = xs

next' :: Zipper' a -> Maybe (Zipper' a)
next' (Z' xs (y:ys) (z:zs)) = Just (Z' (y:xs) (ys ++ [z]) zs)
next' (Z' _  _      []    ) = Nothing
next' (Z' _  []     _     ) = error "This does not happen." -- So data Zipper' should be hidden.

prev' :: Zipper' a -> Maybe (Zipper' a)
prev' (Z' (x:xs) ys zs) = Just (Z' xs (x:(init ys)) ((last ys):zs))
prev' (Z' []     _  _ ) = Nothing

inRange' :: Int -> Int -> Zipper' a -> Zipper' a
inRange' b a (Z' xs ys zs) = Z' (take b xs) ys (take a zs) 


-- mkWordWindow [t0, t1 .. tn] = [ .. Z [n_prev t_k] t_i [n_next t_k'] .. ]
-- "x1 x2 x3 y z1 z2 z3" -> Z [x3,x2,x1] y [z1,z2,z3] 
mkWordWindow :: [Text] -> [Zipper Text]
mkWordWindow xs = (map (inRange nbefore nafter) . catMaybes . fst . break isNothing) inflst
  where izpr = mkZipper xs
        inflst = iterate (join . fmap next) (Just izpr)


mkWordWindow' :: Int -> [a] -> [Zipper' a]
mkWordWindow' n xs = (map (inRange' nbefore nafter) . catMaybes . fst . break isNothing) inflst
  where izpr = mkZipper' n xs
        inflst = iterate (join . fmap next') (Just izpr)

-- for n-words zipper
umkWordWindow' :: [Zipper' a] -> [a]
umkWordWindow' zps = foldr f [] zps ++ (drop 1 $ current' $ last zps)
  where f = (\z acc -> (head $ current' z):acc)

mkWordPosition :: [Text] -> [Int]
mkWordPosition xs = [0..((length xs)-1)]

-- Given regex and normalizer, it takes total list of word window(result of mkWordWindow) as input data.
-- check if the normalized text(= normalizer orig_text) at current cursor position satisfies the given regex.
-- Returns list of regex-satsfied text and its word window.
patternUsages :: (Token -> Text -> Bool)
              -> (Text -> Text)
              -> [Zipper Text]
              -> [(Text, Zipper Text)]
patternUsages matcher normalizer =
  map (\x -> ((normalizer . current) x, x)) . filter ((matcher Number) . current)


-- Text ( -> ["non-Number"]) -> ("non-Number #NUM non-Number #NUM non_Number" :: Text) 
jihunNormalizer :: Text -> Text
jihunNormalizer = T.intercalate "#NUM" . splitNumber

-- Given regex and normalizer, it takes total list of word window(result of mkWordWindow) as input data.
-- It is the same input data with patternUsages. The result of patternUsages is again input of the occurList.
-- As a result, HashMap ("nonN #NUM nonN" :: Key, [Zipper of matched text] :: Value)
collectPatternUsages :: (Token -> Text -> Bool)
                     -> (Text -> Text)
                     -> [Zipper Text]
                     -> HM.HashMap Text [Zipper Text]
collectPatternUsages matcher normalizer = occurList . patternUsages matcher normalizer

-- 
getSNR :: [Int]
       -> Text
       -> HM.HashMap Text [Zipper Text]
       -> HM.HashMap Text [Zipper Text]
       -> Either String (HM.HashMap [Text] (Int,Double))
getSNR col patt sig bkg =
  case (HM.lookup patt sig,HM.lookup patt bkg) of
    (Just sig', Just bkg') -> Right (signalNoiseRatio col sig' bkg')
    (Just sig', Nothing  ) -> Right (onlySignal col sig')
    (Nothing  , Just _   ) -> Left "bkg_only"
    (Nothing  , Nothing  ) -> Left "no_such_pattern"

signalNoiseRatio :: [Int]
                 -> [Zipper Text]
                 -> [Zipper Text]
                 -> HM.HashMap [Text] (Int,Double)
signalNoiseRatio is sig bkg = joinWith f topsig topbkg
  where topsig = topUsage is sig
        topbkg = topUsage is bkg
        f (Just s) (Just b) = (s,fromIntegral s / fromIntegral b)
        f Nothing (Just _)  = (0,0)
        f (Just s) Nothing  = (s,maxratio)  -- for the time being
        f Nothing Nothing   = error "this should not occur"

onlySignal :: [Int]
           -> [Zipper Text]
           -> HM.HashMap [Text] (Int,Double)
onlySignal is sig = HM.map (,maxratio) (topUsage is sig)

-- List of Int (as input) indicates [n0, n1..nN],
-- n_i : n_i th element of latter list (zs), -n_i : n_i th element of former list (xs)
-- word instead of column would be better choice. 
columnFromZipper :: [Int] -> Zipper a -> [a]
columnFromZipper is (Z xs y zs) = map select is
  where select i
          | i < 0     = xs !! (-i-1)
          | i == 0    = y
          | otherwise = zs !! (i-1)

-- Input is [normalized regex-satisfied text]
topUsage :: [Int] -> [Zipper Text] -> HM.HashMap [Text] Int
topUsage is = occurCount . map (columnFromZipper is) 

filterNE :: NETaggerConfig -> [([Text],(Int,Double))] -> [([Text],(Int,Double))]
filterNE NETConfig {..} = take cutoffNTop . filter (\(_,(i,d)) -> (d>cutoffSNRatio) && (i>cutoffSig))

filterCustom :: NETaggerConfig -> [([Text],(Int,Double))] -> [([Text],(Int,Double))]
filterCustom NETConfig {..} = filter (\(_,(i,d)) -> (d < cutoffSNRatio) && (i>cutoffSig))

{-
{ numericPattern = "#NUM"
                       , namedEntityTag = "#!NumberedReference"
                       , cutoffSNRatio = 3
                       , cutoffSig = 10
                       , cutoffNTop = 30
                       }
-}

getNETaggerTable :: NETaggerConfig -> [Zipper Text] -> [Zipper Text]
                 -> NETTagger
getNETaggerTable config' wws_sig wws_bkg =
  let usage_patterns_sig = collectPatternUsages checkTyp jihunNormalizer wws_sig
      usage_patterns_bkg = collectPatternUsages checkTyp jihunNormalizer wws_bkg
      sorter = sortBy (flip compare `on` fst.snd) . HM.toList

      Right snr1 = getSNR [-1] (numericPattern config')
                     usage_patterns_sig usage_patterns_bkg
      snr1_sorted = sorter snr1  
      table1 = filterNE config' snr1_sorted

      -- When you find LEFT2 LEFT1 NUM 
      snr1_remnant = filter (\(_,(i,d)) -> (i > 500) && (d < 10)) snr1_sorted
      table2s =
        flip map snr1_remnant $ \([p],_) ->
          let check = maybe False (== p) . fmap current . prev
              usage_patterns_sig' = HM.map (filter check) usage_patterns_sig
              usage_patterns_bkg' = HM.map (filter check) usage_patterns_bkg
              Right snr2 = getSNR [-2,-1] (numericPattern config')
                             usage_patterns_sig' usage_patterns_bkg'
          in (filterNE config' . sorter) snr2
  in concat (table1 : table2s)

applyNETaggerTable :: [Zipper Text] -> NETTagger -> IO ()
applyNETaggerTable ww patts = do
  forM_ ww $ \w -> do
    let cur = current w
        mprv =  prev w
        p1 = filter (\x -> length x == 1) $ map fst patts
        -- p2 = filter (\x -> length x == 2) $ map fst patts
    case mprv of
      Nothing  -> return ()
      Just prv -> if ((isContainingNumber cur) && ((current prv) `elem` (concat p1)))
        then print $ (current prv,cur)
        else return ()

showResult :: ([Text],(Int,Double)) -> IO ()
showResult (patts,(i,d)) = do
  TIO.putStr ("Pattern: " <> T.intercalate " " patts <> " : ")
  print (i,d)

header :: Text
header = "prefix,re_expr,ne_name,ne_type"

formatPrefix :: [Text] -> Text
formatPrefix patts = T.intercalate "\\s+" patts

formatRegex :: [Text] -> Text
formatRegex patts = "(^|\\s+)" <> formatPrefix patts <> "\\s+\\d+"

formatName :: [Text] -> Text
formatName patts = formatType <> "_" <> formatPrefix patts

formatType :: Text
formatType = "#!NumberedReference"

formatPrint :: [[Text]] -> IO ()
formatPrint pss = TIO.putStrLn header >>  mapM_ (TIO.putStrLn . single) pss
  where
    single ps = T.intercalate "," [formatPrefix ps,formatRegex ps,formatName ps,formatType]

getNERList :: [(Int,Int,Text)] -> [(Text,(Text,Text,Int,Int))]
getNERList mnr =
  let li = reverse $ foldl' (\acc (i,f,t) -> (t,replaceNumber t,i,f):acc) [] mnr
      uidTag uid' i = T.pack $ uid' ++ (show i)
      uid = zipWith uidTag (repeat "UID") [(1 :: Int)..]
  in zip uid li


mergeNR'' :: (Zipper' (Int,Int,Text),Bool) -> ((Int,Int,Text),Bool)
mergeNR'' ptw' =
    let ptw = (current' $ fst ptw')
        ff = (\acc ((_,f,_),(i',_,_)) ->  (T.replicate (i'-f) " "):acc)
        sp = reverse $ foldl' ff [] (zip ptw (drop 1 ptw))
        g  = (\acc ((_,_,t),s) -> T.append acc (T.append t s))
        nr = T.append (foldl' g "" (zip ptw sp)) (last ptw ^. _3)
    in ((head ptw ^. _1,last ptw ^. _2,nr),snd ptw')


replaceTaggedWord :: [(Zipper' (Int,Int,Text),Bool)] -> IO ([(Int,Int,Text)])
replaceTaggedWord ws = do
  rstate <- flip execStateT (ws,[]) $ do
    forM ws $ \_ -> do
      (s,_) <- get
      if (length s == 0)
        then liftIO $ print ("Reached" :: Text)
        else do
          let w = fst $ head s
              b = snd $ head s
              cur = current' w
              i'  = head cur ^. _1
              ff' = head cur ^. _2
              f'  = last cur ^. _2
              t'  = map (\(_,_,t'') -> t'') $ cur
          if b
            then do
            modify' $ (\(s',o') -> (s',(i',f',T.intercalate "" t'):o'))
            modify' $ (\(s',o') -> (drop (length cur) s',o'))
            else do
            modify' $ (\(s',o') -> (s',(i',ff',head t'):o'))
            modify' $ (\(s',o') -> (drop 1 s',o'))
  
  return (reverse $ snd rstate)



replaceTaggedWord' :: [(Zipper' (Int,Int,Text),Bool)] -> IO ([Text])
replaceTaggedWord' ws = do
  rstate <- flip execStateT (ws,[]) $ do
    forM_ ws $ \_ -> do
      (s,_) <- get -- This may be a main killer of performance.
      if (length s == 0)
        then return ()
        else do
          let w = fst $ head s
              b = snd $ head s
              cur = current' w
              t'  = map (\(_,_,t'') -> t'') $ cur
          if b
            then do
            modify $ (\(s',o') -> (s',(T.intercalate "" t'):o'))
            modify $ (\(s',o') -> (drop (length cur) s',o'))
            else do
            modify $ (\(s',o') -> (s',(head t'):o'))
            modify $ (\(s',o') -> (drop 1 s',o'))
  
  return (reverse $ snd rstate)



-- This one has proper performance.
replaceTaggedWord'' :: [(Zipper' (Int,Int,Text),Bool)] -> IO ([Text])
replaceTaggedWord'' ws = do
  let replacePTS txts = T.intercalate "" txts -- T.replace "." " " $ T.intercalate "" txts
  rstate <- flip execStateT (0,[]) $ do
    forM_ ws $ \(w,b) -> do
      (n,_) <- get
      if (n == 0)
        then do
          let cur = current' w
              t'  = map (\(_,_,t'') -> t'') $ cur
          if b
            then modify' $ (\(_ ,o') -> ((length cur)-1, (replacePTS t'):o' ) )
            else modify' $ (\(n',o') -> (n'            , (head t'):o'       ) )
        else do
          modify' $ (\(n',o') -> (n'-1,o'))
          
  return (reverse $ snd rstate)

--------------------------------------------------------------------------------------------------------


getNETaggerTable' :: NETaggerConfig -> [Zipper' Text] -> [Zipper' Text]
                 -> NETTagger
getNETaggerTable' config' wws_sig wws_bkg =
  let usage_patterns_sig = collectPatternUsages' checkTyp jihunNormalizer' wws_sig
      usage_patterns_bkg = collectPatternUsages' checkTyp jihunNormalizer' wws_bkg
      sorter = sortBy (flip compare `on` fst.snd) . HM.toList

      Right snr1 = getSNR' (numericPattern config')
                     usage_patterns_sig usage_patterns_bkg
      snr1_sorted = sorter snr1  
      table1 = filterNE' config' snr1_sorted

      -- When you find LEFT2 LEFT1 NUM 
      snr1_remnant = filter (\(_,(i,d)) -> (i > 500) && (d < 10)) snr1_sorted
      table2s =
        flip map snr1_remnant $ \(p,_) ->
          let check = maybe False (== p) . fmap current' . prev'
              usage_patterns_sig' = HM.map (filter check) usage_patterns_sig
              usage_patterns_bkg' = HM.map (filter check) usage_patterns_bkg
              Right snr2 = getSNR' (numericPattern config')
                             usage_patterns_sig' usage_patterns_bkg'
          in (filterNE' config' . sorter) snr2
  in concat (table1 : table2s)


-- Given regex and normalizer, it takes total list of word window(result of mkWordWindow) as input data.
-- check if the normalized text(= normalizer orig_text) at current cursor position satisfies the given regex.
-- Returns list of regex-satsfied text and its word window.
patternUsages' :: (Token -> Text -> Bool)
              -> ([Text] -> [Text])
              -> [Zipper' Text]
              -> [(Text, Zipper' Text)]
patternUsages' matcher normalizer =
  map (\x -> (T.intercalate " " $ (normalizer . current') x, x)) . filter (\x -> any (matcher Number) $ current' x)


-- Text ( -> ["non-Number"]) -> ("non-Number #NUM non-Number #NUM non_Number" :: Text) 
jihunNormalizer' :: [Text] -> [Text]
jihunNormalizer' = map replaceNumber  -- T.intercalate "#NUM" . splitNumber

-- Given regex and normalizer, it takes total list of word window(result of mkWordWindow) as input data.
-- It is the same input data with patternUsages. The result of patternUsages is again input of the occurList.
-- As a result, HashMap ("nonN #NUM nonN" :: Key, [Zipper of matched text] :: Value)
collectPatternUsages' :: (Token -> Text -> Bool)
                     -> ([Text] -> [Text])
                     -> [Zipper' Text]
                     -> HM.HashMap Text [Zipper' Text]
collectPatternUsages' matcher normalizer = occurList . patternUsages' matcher normalizer

-- Use only "#NUM" (pure number) to get SNR -- Not correct behavior
getSNR' :: Text
        -> HM.HashMap Text [Zipper' Text]
        -> HM.HashMap Text [Zipper' Text]
        -> Either String (HM.HashMap [Text] (Int,Double))
getSNR' patt sig bkg =
  case (HM.lookup patt sig,HM.lookup patt bkg) of
    (Just sig', Just bkg') -> Right (signalNoiseRatio' sig' bkg')
    (Just sig', Nothing  ) -> Right (onlySignal' sig')
    (Nothing  , Just _   ) -> Left "bkg_only"
    (Nothing  , Nothing  ) -> Left "no_such_pattern"


-- ## --
{-
getSNR'' :: [Text]
        -> HM.HashMap [Text] [Zipper' Text]
        -> HM.HashMap [Text] [Zipper' Text] 
        -> Either String (HM.HashMap [Text] (Int,Double))
getSNR'' patt sig bkg =
  case (HM.lookup patt sig,HM.lookup patt bkg) of
    (Just sig', Just bkg') -> Right (signalNoiseRatio'' sig' bkg')
    (Just sig', Nothing  ) -> Right (onlySignal'' sig')
    (Nothing  , Just _   ) -> Left "bkg_only"
    (Nothing  , Nothing  ) -> Left "no_such_pattern"

signalNoiseRatio'' :: [Zipper' Text]
                   -> [Zipper' Text]
                   -> HM.HashMap [Text] (Int,Double)
signalNoiseRatio'' sig bkg = 
-}

-- ## --
signalNoiseRatio' :: [Zipper' Text]
                 -> [Zipper' Text]
                 -> HM.HashMap [Text] (Int,Double)
signalNoiseRatio' sig bkg = joinWith f topsig topbkg
  where topsig = topUsage' sig
        topbkg = topUsage' bkg
        f (Just s) (Just b) = (s,fromIntegral s / fromIntegral b)
        f Nothing (Just _)  = (0,0)
        f (Just s) Nothing  = (s,maxratio)  -- for the time being
        f Nothing Nothing   = error "this should not occur"

onlySignal' :: [Zipper' Text]
           -> HM.HashMap [Text] (Int,Double)
onlySignal' sig = HM.map (,maxratio) (topUsage' sig)

-- Input is [normalized regex-satisfied text]
topUsage' :: [Zipper' Text] -> HM.HashMap [Text] Int
topUsage' = occurCount . map current'   -- map (columnFromZipper' is) 

filterNE' :: NETaggerConfig -> [([Text],(Int,Double))] -> [([Text],(Int,Double))]
filterNE' NETConfig {..} = take cutoffNTop . filter (\(_,(i,d)) -> (d>cutoffSNRatio) && (i>cutoffSig))
