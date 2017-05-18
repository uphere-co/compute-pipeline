{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data Predicate = Predicate { idLang            :: Text
                           , idPOS             :: Text -- id:v or id:n
                           , idPred            :: Text
                           , idRole            :: Text
                           , vnClass           :: Text
                           , vnClassNumber     :: Text
                           , vnSubclass        :: Text
                           , vnSubclassNumber  :: Text
                           , vnLema            :: Text
                           , vnRole            :: Text
                           , wnSense           :: Text
                           , mcrIliOffset      :: Text
                           , fnFrame           :: Text
                           , fnLe              :: Text
                           , fnFrame_Element   :: Text
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
                     , fnFrame_Element   = (x !! 14)
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

main :: IO ()
main = do

  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines

  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == "id:eng") totalmat

  print $ length totalmat
  print $ length $ filter (\x -> idPOS x == "id:v") totalmat 
  print $ length $ filter (\x -> idPOS x == "id:n") totalmat
  print $ length enmat
  print $ length $ filter (\x -> idPOS x == "id:v") enmat 
  print $ length $ filter (\x -> idPOS x == "id:n") enmat

  return ()
