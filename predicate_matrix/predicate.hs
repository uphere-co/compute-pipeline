{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Set     as S
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Predicate = Predicate { idLang            :: Text -- id:cat , id:eng , id:eus , id:spa
                           , idPOS             :: Text -- id:v , id:n
                           , idPred            :: Text -- id: , id:x.y.z
                           , idRole            :: Text -- id:x
                           , vnClass           :: Text -- vn:x.y.z
                           , vnClassNumber     :: Text -- vn:x.y.z
                           , vnSubclass        :: Text -- vn:x.y.z-1
                           , vnSubclassNumber  :: Text -- vn:x.y.z-1
                           , vnLema            :: Text -- vn:w
                           , vnRole            :: Text -- vn:w
                           , wnSense           :: Text -- 
                           , mcrIliOffset      :: Text
                           , fnFrame           :: Text
                           , fnLe              :: Text
                           , fnFrameElement    :: Text
                           , pbRoleset         :: Text
                           , pbArg             :: Text
                           , mcrBC             :: Text
                           , mcrDomain         :: Text
                           , mcrSUMO           :: Text
                           , mcrTO             :: Text
                           , mcrLexname        :: Text
                           , mcrBLC            :: Text
                           , wnSensefrec       :: Text
                           , wnSynsetRelNum    :: Text
                           , esoClass          :: Text
                           , esoRole           :: Text
                           } deriving (Show)

mkPred x = Predicate { idLang            = (x !! 0)
                     , idPOS             = (x !! 1)
                     , idPred            = (x !! 2)
                     , idRole            = (x !! 3)
                     , vnClass           = (x !! 4)
                     , vnClassNumber     = (x !! 5)
                     , vnSubclass        = (x !! 6)
                     , vnSubclassNumber  = (x !! 7)
                     , vnLema            = (x !! 8)
                     , vnRole            = (x !! 9)
                     , wnSense           = (x !! 10)
                     , mcrIliOffset      = (x !! 11)
                     , fnFrame           = (x !! 12)
                     , fnLe              = (x !! 13)
                     , fnFrameElement    = (x !! 14)
                     , pbRoleset         = (x !! 15)
                     , pbArg             = (x !! 16)
                     , mcrBC             = (x !! 17)
                     , mcrDomain         = (x !! 18)
                     , mcrSUMO           = (x !! 19)
                     , mcrTO             = (x !! 20)
                     , mcrLexname        = (x !! 21)
                     , mcrBLC            = (x !! 22)
                     , wnSensefrec       = (x !! 23)
                     , wnSynsetRelNum    = (x !! 24)
                     , esoClass          = (x !! 25)
                     , esoRole           = (x !! 26)
                     }

take' n = S.fromList . take n . S.toList

main :: IO ()
main = do

  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines

  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == "id:eng") totalmat
  {-
  print $ take' 10 $ S.fromList $ map idLang totalmat
  print $ take' 10 $ S.fromList $ map idPOS totalmat
  print $ take' 10 $ S.fromList $ map idPred totalmat
  print $ take' 10 $ S.fromList $ map idRole totalmat
  print $ take' 10 $ S.fromList $ map vnClass totalmat
  print $ take' 10 $ S.fromList $ map vnClassNumber totalmat
  print $ take' 10 $ S.fromList $ map vnSubclass totalmat
  print $ take' 10 $ S.fromList $ map vnSubclassNumber totalmat
  print $ take' 10 $ S.fromList $ map vnLema totalmat
  print $ take' 10 $ S.fromList $ map vnRole totalmat
  print $ take' 50 $ S.fromList $ map wnSense totalmat
  print $ take' 10 $ S.fromList $ map mcrIliOffset totalmat
  print $ take' 10 $ S.fromList $ map fnFrame totalmat
  print $ take' 10 $ S.fromList $ map fnLe totalmat
  print $ take' 10 $ S.fromList $ map fnFrameElement totalmat
  print $ take' 10 $ S.fromList $ map pbRoleset totalmat
  print $ take' 10 $ S.fromList $ map pbArg totalmat
  print $ take' 10 $ S.fromList $ map mcrBC totalmat
  print $ take' 10 $ S.fromList $ map mcrDomain totalmat
  print $ take' 10 $ S.fromList $ map mcrSUMO totalmat
  print $ take' 10 $ S.fromList $ map mcrTO totalmat
  print $ take' 10 $ S.fromList $ map mcrLexname totalmat
  print $ take' 10 $ S.fromList $ map mcrBLC totalmat
  print $ take' 10 $ S.fromList $ map wnSensefrec totalmat
  print $ take' 10 $ S.fromList $ map wnSynsetRelNum totalmat
  print $ take' 10 $ S.fromList $ map esoClass totalmat
  print $ take' 10 $ S.fromList $ map esoRole totalmat
  -}
  
  {-
  PRINT $ length totalmat
  print $ length $ filter (\x -> idPOS x == "id:v") totalmat 
  print $ length $ filter (\x -> idPOS x == "id:n") totalmat
  print $ length enmat
  print $ length $ filter (\x -> idPOS x == "id:v") enmat 
  print $ length $ filter (\x -> idPOS x == "id:n") enmat
  -}
  return ()
