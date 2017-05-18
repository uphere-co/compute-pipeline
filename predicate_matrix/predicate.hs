{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO


data Predicate = Predicate { idLang            :: Text
                           , idPOS             :: Text
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

mkPred x = let {[ idLang , idPOS , idPred , idRole
                , vnClass , vnClassNumber , vnSubclass , vnSubclassNumber , vnLema , vnRole
                , wnSense , mcrIliOffset , fnFrame , fnLe , fnFrame_Element
                , pbRoleset , pbArg , mcrBC , mcrDomain , mcrSUMO , mcrTO , mcrLexname , mcrBLC
                , wnSensefrec , wnSynsetRelNum , esoClass , esoRole ] = x}
           in Predicate {..}

checkData txtss = do
  let len = map length txtss
  print len

main :: IO ()
main = do

  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = T.lines txt
      items = map T.words lines

  let mat = map (\x -> mkPred x) items

  print mat
  putStrLn "Predicate!"
