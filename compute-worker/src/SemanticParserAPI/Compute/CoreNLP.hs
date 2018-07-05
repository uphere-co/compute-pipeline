{-# LANGUAGE OverloadedStrings #-}

module SemanticParserAPI.Compute.CoreNLP where

{- 
import           Data.Aeson
import           Data.Aeson.Encode                         (encodeToBuilder)
import           Data.Aeson.Types                          (typeMismatch)
import qualified Data.Attoparsec.ByteString          as A
import           Data.Binary.Builder                       (toLazyByteString)
import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as BL
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           Network.HTTP.Types                        (methodPost)
import           System.Environment                        (getEnv)
--
import           Network

data NLPResult = NLPResult [Sentence] deriving Show
instance FromJSON NLPResult where
  parseJSON (Object o) = NLPResult <$> o .: "sentences"
  parseJSON invalid = typeMismatch "NLPResult" invalid

data Sentence = Sentence { unSentence :: [Token]} deriving Show

instance FromJSON Sentence where
  parseJSON (Object o) = Sentence <$> o .: "tokens"
  parseJSON invalid = typeMismatch "Sentence" invalid


data Token = Token { unToken :: Text} deriving Show

instance FromJSON Token where
  parseJSON (Object o) = Token <$> o .: "word"
  parseJSON invalid = typeMismatch "Token" invalid

runCoreNLP :: String -> ByteString -> IO ByteString
runCoreNLP corenlp_server body = do
  -- corenlp_server <- getEnv "CORENLPSERVER"
  lbstr <- simpleHttpClient False methodPost (corenlp_server ++"/?properties={%22annotators%22%3A%22depparse%2Cpos%22%2C%22outputFormat%22%3A%22json%22}") (Just body)
  let r_bstr = BL.toStrict lbstr
  let Just c' = do
        Object c <- A.maybeResult (A.parse json r_bstr)
        NLPResult ss <- decodeStrict' r_bstr
        let queries = map (String . T.intercalate " " . map unToken . unSentence) ss 
        return . Object . HM.insert "queries" (Array (V.fromList queries)) $ c
  (return . BL.toStrict . toLazyByteString . encodeToBuilder) c'
-}
