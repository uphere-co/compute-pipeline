{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.Run where

import           Control.Monad                   (forM_)
import qualified Data.ByteString.Char8  as B
import           Data.List                       (sort)
import qualified Data.Text.Lazy.Builder as TLB   (toLazyText)
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Time.Calendar              (fromGregorian,Day)
import           Language.Java          as J
import           Options.Applicative
import           System.Directory                (getDirectoryContents)
import           System.Environment              (getEnv)
import           System.FilePath                 ((</>),takeExtensions)
--
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig),Document(..))
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type.Simplified
--
import           NLP.Type.PennTreebankII
import           HUKB.PPR
import           YAML.Builder
--
import           Annot.NER
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Type

run :: IO ()
run = do
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  opt <- execParser progOption
  -- pgconn <- PGS.connectPostgreSQL (B.pack ("dbname=" ++ dbname opt))
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ sort $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True True
    pp <- prepare pcfg
    mapM_ (process pp forest) filelist -- cnts'
  -- PGS.close pgconn


run2 :: IO ()
run2 = do
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  forest <- prepareForest "/data/groups/uphere/F7745.all_entities" -- (entityFile opt)    
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True)
    forM_ (take 1 filelist) $ \a' -> do
      txt <- getDescription a'
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      -- (r1, r2) <- processDoc ann
      let psents = getProtoSents pdoc
          sents  = convertProtoSents psents pdoc
          tokens = getTokens psents
      print $ mkUkbInput tokens
      process pp forest a'
      TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 tokens))
  putStrLn "Program is finished!"
