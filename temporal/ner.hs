{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Attoparsec.Text
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree
import           System.Console.Haskeline
--
import           SearchTree

testtxt = "Some people are deeply skeptical that creating a new hybrid class of devices will \
          \help stop the momentum of tablets from Apple and companies with devices based \
          \on Google’s Android operating system. Marc Benioff, the chief executive of Salesforce.com \
          \and a frequent Microsoft antagonist, said customers had already shunned new types of devices, \
          \like Microsoft’s Surface."

skipTill :: Alternative f => f a -> f b -> f b
skipTill p end = scan
  where scan = end <|> (p *> scan)

pTree forest acc = do
  let lst = searchForest acc forest
  case lst of
    [] -> return acc
    _ -> do
      x <- satisfy (inClass lst)
      pTree forest (acc++[x])

pTreeAdv forest = skipTill anyChar (pTree forest [])
  
main :: IO ()
main = do
  putStrLn "search"  
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst

  let forest = foldr addTreeItem [] nentities

  print $ parseOnly (many (pTreeAdv forest)) testtxt
