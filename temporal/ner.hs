{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Internal.Types as AT
import           Data.Maybe          (isJust)
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree
import           System.Console.Haskeline
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getArgs)
--
import           SearchTree
import           Type
import           Util.Doc
import           View

{- 
testtxt = "Some people are deeply skeptical that creating a new hybrid class of devices will \
          \help stop the momentum of tablets from Apple and companies with devices based \
          \on Google’s Android operating system. Marc Benioff, the chief executive of Salesforce.com \
          \and a frequent Microsoft antagonist, said customers had already shunned new types of devices, \
          \like Microsoft’s Surface."
-}

getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

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

pTreeAdv forest = skipTill anyChar p
  where p = do
          b <- getPos
          x <- pTree forest []
          e <- getPos
          return (b+1,e,x)

prepareForest = do
  txt <- TIO.readFile "F7745.all_entities"
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
  return (foldr addTreeItem [] nentities)
  
        
main :: IO ()
main = do
  forest <- prepareForest
  args <- getArgs
  let dir = args !! 0
  -- putStrLn "---------------------------"
  -- TIO.putStrLn testtxt
  -- putStrLn "---------------------------"
  cnts <- getDirectoryContents dir
  let cnts' = map (dir </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
  mapM_ (process forest) cnts'


process :: Forest Char -> FilePath -> IO ()
process forest fp = do
  txt <- TIO.readFile fp  
  case parseOnly (many (pTreeAdv forest)) txt of
    Left err -> print err
    Right parsed -> do
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn "-----------------------------------------------------------"
      let f (b,e,_) = ((),b,e)
          tagged = map f parsed
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="
