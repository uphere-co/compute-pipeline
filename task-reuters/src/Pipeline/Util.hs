{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


module Pipeline.Util where

import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Either       (EitherT,runEitherT,hoistEither)
import           Data.Attoparsec.Text             (parseOnly)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Discrimination              (outer)
import           Data.Discrimination.Grouping     (hashing)
import           Data.Maybe                       (fromJust, isJust)
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar               (Day)
import           Data.Time.Clock                  (UTCTime)
import           Data.Time.Format                 (defaultTimeLocale, formatTime, parseTimeM)
import           Data.Tree
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (createDirectoryIfMissing,doesFileExist)
import           System.FilePath                  ((</>),takeDirectory,takeFileName)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Util
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import           NLP.Type.CoreNLP
import           Pipeline.Type
import           Text.Annotation.Util.Doc
import           Text.Search.SearchTree
--
import           NLP.Type.PennTreebankII
import           WordNet.Type.POS
import qualified WordNet.Type.POS          as WordNet
import           System.Console.Haskeline
--


splitPrefixSubDirs :: FilePath -> (FilePath, FilePath, String)
splitPrefixSubDirs fp =
  let hsh       = takeFileName fp
      storepath = takeDirectory fp
      prefix    = take 2 hsh
  in (hsh,storepath,prefix)

saveHashNameBSFileInPrefixSubDirs :: FilePath -> ByteString -> IO ()
saveHashNameBSFileInPrefixSubDirs fp file = do
  let (hsh,storepath,prefix) = splitPrefixSubDirs fp
  createDirectoryIfMissing True (storepath </> prefix)
  B.writeFile (storepath </> prefix </> hsh) file

saveHashNameTextFileInPrefixSubDirs :: FilePath -> Text -> IO ()
saveHashNameTextFileInPrefixSubDirs fp file = do
  let (hsh,storepath,prefix) = splitPrefixSubDirs fp
  createDirectoryIfMissing True (storepath </> prefix)
  TIO.writeFile (storepath </> prefix </> hsh) file

doesHashNameFileExistInPrefixSubDirs :: FilePath -> IO Bool
doesHashNameFileExistInPrefixSubDirs fp = do
  let (hsh,storepath,prefix) = splitPrefixSubDirs fp
  b <- doesFileExist (storepath </> prefix </> hsh)
  return b

unB16 :: String -> ByteString
unB16 = fst . B16.decode . BL8.toStrict . BL8.pack


digitsToUTC :: String -> Maybe UTCTime
digitsToUTC str = parseTimeM True defaultTimeLocale "%Y%m%d" str
  
