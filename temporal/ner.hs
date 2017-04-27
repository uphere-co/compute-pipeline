{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad                        (void)
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Internal.Types as AT
import           Data.Maybe                           (isJust)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Tree
import qualified Options.Applicative            as O
import           System.Console.Haskeline
import           System.Directory                     (getDirectoryContents)
import           System.FilePath                      ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment                   (getArgs)
--
import           SearchTree
import           Type
import           Util.Doc
import           View

data ProgOption = ProgOption { dir :: FilePath
                             , entityFile :: FilePath
                             } deriving Show

pOptions :: O.Parser ProgOption
pOptions = ProgOption <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "Directory")
                      <*> O.strOption (O.long "entity" <> O.short 'e' <> O.help "Entity File")

progOption :: O.ParserInfo ProgOption 
progOption = O.info pOptions (O.fullDesc <> O.progDesc "Named Entity Recognition")

getPos :: Parser Int
getPos = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

skipTill :: Alternative f => f a -> f b -> f b
skipTill p end = scan
  where scan = end <|> (p *> scan)

tokencloser :: Parser ()
tokencloser = void (satisfy (`elem` (" .,!?:;()-+=\"'`/\\|" :: String))) <|> endOfInput

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

pTreeAdv forest = skipTill anyChar p
  where p = do
          b <- getPos
          (x,e) <- pTree forest []
          return (b+1,e,x)

prepareForest fp = do
  txt <- TIO.readFile fp
  let lst = map ((\(a,b) -> (a,T.drop 1 b)) . T.breakOn "\t") . T.lines $ txt
      nentities = map (T.unpack . snd) lst
  return (foldr addTreeItem [] nentities)


main :: IO ()
main = do
  opt <- O.execParser progOption
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
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
