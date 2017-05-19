{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Source.NewsAPI.Article where

import qualified Data.ByteString.Char8      as B
import           Data.Aeson                       (eitherDecodeStrict)
import           Data.Text                        (Text)
--
import           Intrinio.Type

getDescription :: FilePath -> IO Text
getDescription f = do
    bstr <- B.readFile f
    let ea = eitherDecodeStrict bstr :: Either String SourceArticles
    case ea of
      Left  _ -> return ""
      Right a -> return (maybe "" id (_description a))
