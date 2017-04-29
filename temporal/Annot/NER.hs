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


getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

skipTill :: Alternative f => f a -> f b -> f b
skipTill p end = scan'
  where scan' = end <|> (p *> scan')

tokencloser :: Parser ()
tokencloser = void (satisfy (`elem` (" .,!?:;()-+=\"'`/\\|" :: String))) <|> endOfInput

pTree :: Forest Char -> [Char] -> Parser (String,Int)
pTree forest acc = do
  let lst = searchForest acc forest
  case lst of
    [] -> do
      e <- getPos 
      tokencloser
      return (acc,e)
    _ -> do
      x <- satisfy (\c -> c `elem` lst)
      pTree forest (acc++[x])

pTreeAdv :: Forest Char -> Parser (Int,Int,String)
pTreeAdv forest = skipTill anyChar p
  where p = do
          b <- getPos
          (x,e) <- pTree forest []
          return (b+1,e,x)

prepareForest :: FilePath -> IO (Forest Char)
prepareForest fp = do
  txt <- TIO.readFile fp
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
  return (foldr addTreeItem [] nentities)



