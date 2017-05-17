{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.Run where

--
import           Options.Applicative
import           Data.List                        (sort)
import           System.FilePath                  ((</>),takeExtensions)
import           Control.Monad                    (forM_)
import qualified Data.ByteString.Char8      as B
import           Language.Java         as J
import           System.Environment               (getEnv)
import Pipeline.Util
--
import Annot.NER
--
import           CoreNLP.Simple.Type     (PipelineConfig(PPConfig),Document(..))
import           CoreNLP.Simple          (annotate,prepare)

import           Data.Time.Calendar               (fromGregorian,Day)
import           System.Directory                 (getDirectoryContents)

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
    let pcfg = PPConfig True True True True True
    pp <- prepare pcfg

    forM_ (take 1 filelist) $ \a' -> do
      txt <- getDescription a'
      parseSen txt pp

      let doc = Document txt (fromGregorian 2017 4 17) 
      ann <- annotate pp doc
      (r1, r2) <- processDoc ann
      return ()
      -- print $ filter (\(_,y) -> y /= "U") $ zip (map _token_lemma r2) (map simpleMap $ map _token_pos r2)
      -- process pp forest a'
  putStrLn "Program is finished!"
