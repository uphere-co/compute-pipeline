{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.UKB where

import qualified Data.ByteString.Char8  as B
import           System.FilePath             ((</>))
import           Language.Java          as J
import qualified Data.Text              as T
import           System.Environment              (getEnv)
--
import           HUKB.PPR
--
import           Pipeline.Util
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))

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

runWSD txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      sents  = convertProtoSents psents pdoc
      tokens = getTokens psents
  (_,xs) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
  return xs
