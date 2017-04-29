{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                    (toList)
import           Data.Maybe                       (isJust)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           Type
import           Util.Doc
import           View
--
import           CoreNLP.Simple
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
--
import           Annot.SUTime


data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: Options.Applicative.Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "sutime")


main :: IO ()
main = do
  pgconn <- PGS.connectPostgreSQL "dbname=nytimes"
  opt <- execParser progOption
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True
    pp <- prepare pcfg
    mapM_ (process pgconn pp) cnts'
  PGS.close pgconn

process :: PGS.Connection -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> FilePath -> IO ()
process pgconn pp fp = do
  let sha256 = takeBaseName fp
  d <- getArticlePubDay pgconn (B.pack sha256)
  txt <- TIO.readFile fp
  ann <- annotate pp txt (T.pack d) 
  bstr <- serializeTimex ann
  let lbstr = BL.fromStrict bstr
  case (messageGet lbstr :: Either String (T.ListTimex,BL.ByteString)) of
    Left err -> print err
    Right (r,_lbstr') -> do
      let tmxs = toList (r^.T.timexes)
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn $ "date: " ++ d
      putStrLn "-----------------------------------------------------------"
      mapM_ (TIO.putStrLn . format) tmxs
      putStrLn "-----------------------------------------------------------"
      let f t = ((),fromIntegral (t^.T.characterOffsetBegin+1), fromIntegral (t^.T.characterOffsetEnd))
          tagged = fmap f tmxs
      let anntxt = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 anntxt
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="

  

