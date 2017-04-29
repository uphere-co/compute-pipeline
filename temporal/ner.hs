{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Maybe                           (isJust)
import           Data.Monoid                          ((<>))
import qualified Data.Text.IO                   as TIO
import           Data.Tree
import qualified Options.Applicative            as O
import           System.Directory                     (getDirectoryContents)
import           System.FilePath                      ((</>),takeExtensions,takeFileName)
--
import           Type
import           Util.Doc
import           View
--
import           Annot.NER


data ProgOption = ProgOption { dir :: FilePath
                             , entityFile :: FilePath
                             } deriving Show

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")
                      <*> O.strOption (O.long "entity" <> O.short 'e' <> O.help "Entity File")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "Named Entity Recognition")


main :: IO ()
main = do
  opt <- O.execParser progOption
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
  mapM_ (process forest) (Prelude.take 5 cnts')


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
