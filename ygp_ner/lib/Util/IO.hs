{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.IO where

import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               as CSV
import           Data.List                     (intersperse)
import           Data.Text                     (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.IO      as TLIO
import qualified Data.Vector            as V
--
import Type


-- Temporarily reads only Regulation.
readSigFile :: String -> IO (Text)
readSigFile filepath = do
  csvData <- BL.readFile filepath
  case (decodeByName csvData :: Either String (Header, V.Vector Regulation)) of
    Left err -> error err
    Right (_, v) -> do
      let vs = V.toList v
          sigtxt = T.intercalate "\n\n#ROW_BEGIN#\n\n" . filter (not . T.null) . map enmainrequire $ vs
      return sigtxt

readBkgFile :: String -> IO ([Text])
readBkgFile filepath = do
  bkgtxt <- TLIO.readFile filepath
  let ws0_bkg :: [[Text]]
      ws0_bkg = (map (map TL.toStrict . TL.words) . take bkgLines . TL.lines) bkgtxt
      ws_bkg = (concat . intersperse ["#ROW_BEGIN#"]) ws0_bkg
  return ws_bkg

readNETableFile :: String -> IO ([([Text], (Int, Double))])
readNETableFile filepath = do
  tbltxt <- TLIO.readFile filepath
  let (tbl :: [([Text], (Int, Double))]) = read (TL.unpack tbltxt)
  return tbl

readIndicatorFile :: String -> IO ([Text])
readIndicatorFile filepath = do
  tbltxt <- TLIO.readFile filepath
  let (tbl :: [Text]) = read (TL.unpack tbltxt)
  return tbl

readAnnotSenFile :: String -> IO ([(Int,Int)])
readAnnotSenFile filepath = do
  annottxt <- TLIO.readFile filepath
  let (annot :: [(Int,Int)]) = read (TL.unpack annottxt)
  return annot
