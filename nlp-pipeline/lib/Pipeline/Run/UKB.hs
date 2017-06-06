{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Pipeline.Run.UKB where

import qualified Data.ByteString.Char8  as B
import           System.FilePath             ((</>))
import qualified Data.Text              as T

import           Foreign.JNI.Types
--
import           HUKB.PPR
--
import           Pipeline.Util
import           CoreNLP.Simple                  (annotate)

runPPR :: String -> IO ()
runPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  print result

getPPR :: String -> IO (B.ByteString,[(B.ByteString, B.ByteString, B.ByteString, B.ByteString)])
getPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  return result

getWSD :: T.Text
       -> Foreign.JNI.Types.J
          ('Foreign.JNI.Types.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
       -> IO [(B.ByteString, B.ByteString, B.ByteString, B.ByteString)]
getWSD txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      tokens = getAllTokens psents
  (_,xs) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
  return xs
