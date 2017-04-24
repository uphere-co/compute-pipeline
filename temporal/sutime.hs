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
import           Control.Monad                    ((>=>),join,void)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Loops              (whileJust_)
import           Control.Monad.Trans.Class        (lift)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe                       (isJust)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           GHC.Generics
import           Language.Java         as J
import           Options.Applicative
import           System.Console.Haskeline
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeExtensions)
import           System.Environment               (getEnv,getArgs)
import           Text.Printf 
--
import           Type
import           Util.Doc
import           View
--
import           CoreNLP
import           CoreNLP.SUTime
import           CoreNLP.SUTime.Parser
import           CoreNLP.Type


data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: Options.Applicative.Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")

progOption = info pOptions (fullDesc <> progDesc "sutime")

formatstr n x = T.pack (printf ("%" ++ show n ++ "s") x)
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


process pp fp = do
  txt <- TIO.readFile fp
  r <- annotateTime pp txt "2017-04-17"
  TIO.putStrLn r
  case A.parseOnly (many (timetag <* A.skipSpace)) r of
    Left err -> print err
    Right xs -> do 
      TIO.putStrLn txt
      putStrLn "==========================================================="
      mapM_ (TIO.putStrLn . format) xs
      putStrLn "==========================================================="
      let f ttag = ((), ttag^.coffbeg  + 1, ttag^.coffend)
          tagged = map f xs 
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      flip mapM_ xss $ \xs -> mapM_ cutePrintAnnot xs
 

