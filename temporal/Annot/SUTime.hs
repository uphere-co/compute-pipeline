{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Annot.SUTime where

import           Control.Lens
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Time as PGS
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.Time.Format
import           Data.Time.LocalTime              (zonedTimeToUTC)
import           Text.Printf
import           Text.ProtocolBuffers.Basic       (Utf8, utf8)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Timex as Tmx
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
--


formatstr :: Int -> Text -> Text
formatstr n x = T.pack (printf ("%" ++ show n ++ "s") x)

format :: T.TimexWithOffset -> Text
format x = T.pack (show (x ^. T.characterOffsetBegin)) <> "\t" <>
           T.pack (show (x ^. T.characterOffsetEnd)) <> "\t" <>
           formatstr 20 (cutf8 (x ^. T.timex . Tmx.text)) <> "\t" <>
           cutf8 (x ^. T.timex . Tmx.value)

cutf8 :: Maybe Utf8 -> Text
cutf8 = TL.toStrict . TLE.decodeUtf8 . fromMaybe ""  . fmap utf8 
           
getArticlePubDay :: PGS.Connection -> B.ByteString -> IO String
getArticlePubDay conn sha256 = do
  let idbstr = fst (B16.decode sha256) 
  [r] :: [Maybe (PGS.Only PGS.ZonedTimestamp)] <- PGS.query conn "select published from article where sha256 = ?" (PGS.Only (PGS.Binary idbstr))
  case r of
    Nothing -> return "2099-01-01"
    Just (PGS.Only i) -> 
      let PGS.Finite t = i
      in return $ formatTime defaultTimeLocale "%F" (zonedTimeToUTC t)
