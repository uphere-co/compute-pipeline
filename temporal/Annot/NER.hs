{-# LANGUAGE OverloadedStrings #-}

module Annot.NER where

import           Control.Applicative
import           Control.Monad                        (void)
import           Data.Attoparsec.Text                 
import qualified Data.Attoparsec.Internal.Types as AT (Parser(..),fromPos)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Tree
--
import           SearchTree



prepareForest :: FilePath -> IO (Forest (Maybe Char))
prepareForest fp = do
  txt <- TIO.readFile fp
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
  return (foldr addTreeItem [] nentities)



