{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.Run where

import           Control.Applicative
import           Control.Lens                    ((^.),_2,_3)
import           Control.Monad                   (forM_)
import           Control.Monad.Trans.Either      (EitherT(..),left,right,hoistEither)
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Char8  as B
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB   (toLazyText)
import qualified Data.Text.Lazy.IO      as TLIO
import           Language.Java          as J
import           System.Environment              (getEnv)
import           System.FilePath                 ((</>))
--
import           Annot.NER
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
--
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple                  (annotate,prepare)
import           HUKB.PPR
import           YAML.Builder
import           WordNet.API.Query
import           WordNet.Type
import           PM.API.Query
import           Generic.SearchTree
import           ParserCustom

runPPR :: String -> IO ()
runPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  print result

getPPR :: String
       -> IO (B.ByteString,[(B.ByteString, B.ByteString, B.ByteString, B.ByteString)])
getPPR txt = do
  let dir = "/nix/store/c61cbi65n9ifia3xinxcq5r5jqd1gbyn-ukb-3.0/share/data"
  result <- ppr (dir </> "wn30.bin") (dir </> "wnet30_dict.txt") "ctx_01" txt
  return result
  
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
      -- print $ mkUkbInput tokens
      -- runPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      -- process pp forest a'
      -- TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 tokens))
      -- getTemporal ann
      (_,xs) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      db <- loadDB "/data/groups/uphere/data/NLP/dict"
      pmdata <- loadPM "/data/groups/uphere/data/NLP/PredicateMatrix.v1.3.txt"
      let pm = createPM pmdata
      forest <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"
      let s = runState (runEitherT (many $ pTreeAdvG forest)) (map T.unpack $ T.words txt) -- ["as","long","as","possible","take","care","of","away","from"]
      print s
      {-
      forM_ xs $ \x -> do
        runSingleQuery (B.unpack $ (x ^. _3)) (convStrToPOS $ B.unpack $ (x ^. _2)) db
        print $ query (T.pack $ B.unpack $ (x ^. _3)) pm
      -}
  putStrLn "Program is finished!"
