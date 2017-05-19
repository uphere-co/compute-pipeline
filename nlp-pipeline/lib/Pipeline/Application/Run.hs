{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.Run where

import           Control.Monad                   (forM_)
import qualified Data.ByteString.Char8  as B
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB   (toLazyText)
import qualified Data.Text.Lazy.IO      as TLIO
import           Language.Java          as J
import           System.Environment              (getEnv)
import           System.FilePath                 ((</>))
--
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple                  (annotate,prepare)
-- import           CoreNLP.Simple.Type.Simplified
-- import           NLP.Type.PennTreebankII
import           HUKB.PPR
import           YAML.Builder
--
import           Annot.NER
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util

runPPR :: String -> IO ()
runPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt

run :: IO ()
run = do
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  forest <- prepareForest "/data/groups/uphere/F7745.all_entities"
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True)
    forM_ filelist $ \a' -> do
      txt <- getDescription a'
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          sents  = convertProtoSents psents pdoc
          tokens = getTokens psents
      print $ sents
      print $ mkUkbInput tokens
      runPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      process pp forest a'
      TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 tokens))
      getTemporal ann
  putStrLn "Program is finished!"
