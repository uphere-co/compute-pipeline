{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO


data Predicate = Predicate { iD_LANG            :: Text
                           , iD_POS             :: Text
                           , iD_PRED            :: Text
                           , iD_ROLE            :: Text
                           , vN_CLASS           :: Text
                           , vN_CLASS_NUMBER    :: Text
                           , vN_SUBCLASS        :: Text
                           , vN_SUBCLASS_NUMBER :: Text
                           , vN_LEMA            :: Text
                           , vN_ROLE            :: Text
                           , wN_SENSE           :: Text
                           , mCR_iliOffset      :: Text
                           , fN_FRAME           :: Text
                           , fN_LE              :: Text
                           , fN_FRAME_ELEMENT   :: Text
                           , pB_ROLESET         :: Text
                           , pB_ARG             :: Text
                           , mCR_BC             :: Text
                           , mCR_DOMAIN         :: Text
                           , mCR_SUMO           :: Text
                           , mCR_TO             :: Text
                           , mCR_LEXNAME        :: Text
                           , mCR_BLC            :: Text
                           , wN_SENSEFREC       :: Text
                           , wN_SYNSET_REL_NUM  :: Text
                           , eSO_CLASS          :: Text
                           , eSO_ROLE           :: Text
                           } deriving (Show)


checkData txtss = do
 let len = map length txtss
 print len

main :: IO ()
main = do

  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = T.lines txt
      items = map T.words lines

  let mat = map (\x -> f x) items
      f x = Predicate { iD_LANG            = (x !! 0)            
                      , iD_POS             = (x !! 1) 
                      , iD_PRED            = (x !! 2)
                      , iD_ROLE            = (x !! 3)
                      , vN_CLASS           = (x !! 4)
                      , vN_CLASS_NUMBER    = (x !! 5)
                      , vN_SUBCLASS        = (x !! 6)
                      , vN_SUBCLASS_NUMBER = (x !! 7)
                      , vN_LEMA            = (x !! 8)
                      , vN_ROLE            = (x !! 9)
                      , wN_SENSE           = (x !! 10)
                      , mCR_iliOffset      = (x !! 11)
                      , fN_FRAME           = (x !! 12)
                      , fN_LE              = (x !! 13)
                      , fN_FRAME_ELEMENT   = (x !! 14)
                      , pB_ROLESET         = (x !! 15)
                      , pB_ARG             = (x !! 16)
                      , mCR_BC             = (x !! 17)
                      , mCR_DOMAIN         = (x !! 18)
                      , mCR_SUMO           = (x !! 19)
                      , mCR_TO             = (x !! 20)
                      , mCR_LEXNAME        = (x !! 21)
                      , mCR_BLC            = (x !! 22)
                      , wN_SENSEFREC       = (x !! 23)
                      , wN_SYNSET_REL_NUM  = (x !! 24)
                      , eSO_CLASS          = (x !! 25)
                      , eSO_ROLE           = (x !! 26)
                      }
  print mat
  putStrLn "Predicate!"
