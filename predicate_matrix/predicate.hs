{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Set     as S
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO



data Lang = Cat | Eng | Eus | Spa deriving (Eq,Show)
data POS  = Verb | Noun deriving (Eq,Show)

type Predicate = Text
type Role      = Text




data PredicateMatrix = PM { idLang            :: Lang      -- id:cat , id:eng , id:eus , id:spa
                          , idPOS             :: POS       -- id:v , id:n
                          , idPred            :: Predicate -- id: , id:x.y.z
                          , idRole            :: Role      -- id:x
                          , vnClass           :: Text      -- vn:x.y.z
                          , vnClassNumber     :: Text      -- vn:x.y.z
                          , vnSubclass        :: Text      -- vn:x.y.z-1
                          , vnSubclassNumber  :: Text      -- vn:x.y.z-1
                          , vnLema            :: Text      -- vn:w
                          , vnRole            :: Text      -- vn:w
                          , wnSense           :: Text 
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

getLang x = case x of
  "id:cat" -> Cat
  "id:eng" -> Eng
  "id:eus" -> Eus
  "id:spa" -> Spa  

getPOS x = case x of
  "id:n" -> Noun
  "id:v" -> Verb

getPredicate :: Text -> Predicate
getPredicate txt = last $ T.splitOn ":" txt

getRole :: Text -> Role
getRole txt = last $ T.splitOn ":" txt

getVNClass txt = last $ T.splitOn ":" txt
getVNClassNumber txt = last $ T.splitOn ":" txt
getVNSubclass txt = last $ T.splitOn ":" txt
getVNSubclassNumber txt = last $ T.splitOn ":" txt
getVNLema txt = last $ T.splitOn ":" txt
getVNRole txt = last $ T.splitOn ":" txt

getWord txt = last $ T.splitOn ":" txt

mkPred x = PM { idLang            = getLang (x !! 0)
              , idPOS             = getPOS (x !! 1)
              , idPred            = getPredicate (x !! 2)
              , idRole            = getRole (x !! 3)
              , vnClass           = getWord (x !! 4)
              , vnClassNumber     = getWord (x !! 5)
              , vnSubclass        = getWord (x !! 6)
              , vnSubclassNumber  = getWord (x !! 7)
              , vnLema            = getWord (x !! 8)
              , vnRole            = getWord (x !! 9)
              , wnSense           = getWord (x !! 10)
              , mcrIliOffset      = getWord (x !! 11)
              , fnFrame           = getWord (x !! 12)
              , fnLe              = getWord (x !! 13)
              , fnFrameElement    = getWord (x !! 14)
              , pbRoleset         = getWord (x !! 15)
              , pbArg             = getWord (x !! 16)
              , mcrBC             = getWord (x !! 17)
              , mcrDomain         = getWord (x !! 18)
              , mcrSUMO           = getWord (x !! 19)
              , mcrTO             = getWord (x !! 20)
              , mcrLexname        = getWord (x !! 21)
              , mcrBLC            = getWord (x !! 22)
              , wnSensefrec       = getWord (x !! 23)
              , wnSynsetRelNum    = getWord (x !! 24)
              , esoClass          = getWord (x !! 25)
              , esoRole           = getWord (x !! 26)
              }
  
take' n = S.fromList . take n . S.toList

main :: IO ()
main = do

  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines

  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat

  
  print $ S.fromList $ map idRole totalmat


  print (take 10 enmat)

  

  {-
  print $ take' 50 $ S.fromList $ map idPred totalmat
  print $ take' 50 $ S.fromList $ map idRole totalmat
  print $ take' 50 $ S.fromList $ map vnClass totalmat
  print $ take' 50 $ S.fromList $ map vnClassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnSubclass totalmat
  print $ take' 50 $ S.fromList $ map vnSubclassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnLema totalmat
  print $ take' 50 $ S.fromList $ map vnRole totalmat
  print $ take' 50 $ S.fromList $ map wnSense totalmat
  print $ take' 50 $ S.fromList $ map mcrIliOffset totalmat
  print $ take' 50 $ S.fromList $ map fnFrame totalmat
  print $ take' 50 $ S.fromList $ map fnLe totalmat
  print $ take' 50 $ S.fromList $ map fnFrameElement totalmat
  print $ take' 50 $ S.fromList $ map pbRoleset totalmat
  print $ take' 50 $ S.fromList $ map pbArg totalmat
  print $ take' 50 $ S.fromList $ map mcrBC totalmat
  print $ take' 50 $ S.fromList $ map mcrDomain totalmat
  print $ take' 50 $ S.fromList $ map mcrSUMO totalmat
  print $ take' 50 $ S.fromList $ map mcrTO totalmat
  print $ take' 50 $ S.fromList $ map mcrLexname totalmat
  print $ take' 50 $ S.fromList $ map mcrBLC totalmat
  print $ take' 50 $ S.fromList $ map wnSensefrec totalmat
  print $ take' 50 $ S.fromList $ map wnSynsetRelNum totalmat
  print $ take' 50 $ S.fromList $ map esoClass totalmat
  print $ take' 50 $ S.fromList $ map esoRole totalmat
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
