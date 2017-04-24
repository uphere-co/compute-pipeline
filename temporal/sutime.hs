{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Applicative
import           Control.Lens
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8 as B
import           Data.Maybe                       (isJust)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.Printf 
--
import           Type
import           Util.Doc
import           View
--
import           CoreNLP.SUTime
import           CoreNLP.SUTime.Parser


data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: Options.Applicative.Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "sutime")

formatstr :: Int -> Text -> Text
formatstr n x = T.pack (printf ("%" ++ show n ++ "s") x)

format :: TimeTag -> Text
format x = T.pack (show (x ^. coffbeg)) <> "\t" <> T.pack (show (x ^. coffend)) <> "\t" <> formatstr 20 (x ^. text) <> "\t" <> x ^. timex

  
main :: IO ()
main = do
  opt <- execParser progOption
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare
    mapM_ (process pp) cnts'

process :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> FilePath -> IO ()
process pp fp = do
  txt <- TIO.readFile fp
  r <- annotateTime pp txt "2017-04-17"
  -- TIO.putStrLn r
  case A.parseOnly (many (timetag <* A.skipSpace)) r of
    Left err -> print err
    Right xs -> do 
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp 
      putStrLn "-----------------------------------------------------------"
      mapM_ (TIO.putStrLn . format) xs
      putStrLn "-----------------------------------------------------------"
      let f ttag = ((), ttag^.coffbeg  + 1, ttag^.coffend)
          tagged = map f xs 
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="
 

