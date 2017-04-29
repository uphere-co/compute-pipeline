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
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                    (toList)
import           Data.Maybe                       (fromMaybe,isJust)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Time as PGS
-- import qualified Database.PostgreSQL.Simple.Types as PGS
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock                  (utctDay)
import           Data.Time.Format
import           Data.Time.LocalTime              (zonedTimeToUTC)
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.Printf
import           Text.ProtocolBuffers.Basic       (utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           Type
import           Util.Doc
import           View
--
-- import           CoreNLP.SUTime
-- import           CoreNLP.SUTime.Parser
import           CoreNLP.Simple
import qualified CoreNLP.Proto.CoreNLPProtos.Timex as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T

data ProgOption = ProgOption { dir :: FilePath } deriving Show

pOptions :: Options.Applicative.Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "sutime")

formatstr :: Int -> Text -> Text
formatstr n x = T.pack (printf ("%" ++ show n ++ "s") x)

format :: T.TimexWithOffset -> Text
format x = T.pack (show (x ^. T.characterOffsetBegin)) <> "\t" <>
           T.pack (show (x ^. T.characterOffsetEnd)) <> "\t" <>
           formatstr 20 (cutf8 (x ^. T.timex . Tmx.text)) <> "\t" <>
           cutf8 (x ^. T.timex . Tmx.value)

cutf8 = TL.toStrict . TLE.decodeUtf8 . fromMaybe ""  . fmap utf8 
           
getArticlePubDay :: PGS.Connection -> B.ByteString -> IO String
getArticlePubDay conn sha256 = do
  let idbstr = fst (B16.decode sha256) 
  [r] :: [Maybe (PGS.Only PGS.ZonedTimestamp)] <- PGS.query conn "select published from article where sha256 = ?" (PGS.Only (PGS.Binary idbstr))
  -- q :: [Maybe (PGS.Only PGS.ZonedTimestamp)] <- PGS.query conn "select modified from article where sha256 = ?" (PGS.Only (PGS.Binary idbstr))
  case r of
    Nothing -> return "2099-01-01"
    Just (PGS.Only i) -> 
      let PGS.Finite t = i
      in return $ formatTime defaultTimeLocale "%F" (zonedTimeToUTC t)

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
  -- TIO.putStrLn txt
  ann <- annotate pp txt (T.pack d) 
  bstr <- serializeTimex ann
  let lbstr = BL.fromStrict bstr
  case (messageGet lbstr :: Either String (T.ListTimex,BL.ByteString)) of
    Left err -> print err
    Right (r,lbstr') -> do
      let tmxs = toList (r^.T.timexes)
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn $ "date: " ++ d
      putStrLn "-----------------------------------------------------------"
      mapM_ (TIO.putStrLn . format) tmxs
      putStrLn "-----------------------------------------------------------"
      let f t = ((),fromIntegral (t^.T.characterOffsetBegin+1), fromIntegral (t^.T.characterOffsetEnd))
          tagged = fmap f tmxs
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="

  
  {- 
  r <- annotateTime pp txt (T.pack d) 
  case A.parseOnly (many (timetag <* A.skipSpace)) r of
    Left err -> print err
    Right xs -> do 
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn $ "date: " ++ d
      putStrLn "-----------------------------------------------------------"
      mapM_ (TIO.putStrLn . format) xs
      putStrLn "-----------------------------------------------------------"
      let f ttag = ((), ttag^.coffbeg  + 1, ttag^.coffend)
          tagged = map f xs 
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="
   -}

