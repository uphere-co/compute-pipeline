{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception          (SomeException,try)
import           Control.Lens
import           Control.Monad              (forM_,replicateM_,void,when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Database.PostgreSQL.Simple as PGS
import           Language.Java              as J
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           NewsAPI.DB                 (uploadAnalysis)
import qualified NewsAPI.DB.Article         as A
import           NewsAPI.Type               (NewsAPIAnalysisDB(..))
import           OntoNotes.App.Analyze
import           OntoNotes.App.Analyze.SentenceStructure
--
import           Pipeline.App.CoreNLPRunner
import           Pipeline.Load
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Util

-- Load and Run
main' :: IO ()
main' = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  loaded' <- loadCoreNLPResult "/home/modori/data/newsapianalyzed"
  putStrLn "Loading Completed."
  let loaded = catMaybes loaded'
  forM_ loaded $ \x -> do
    mapM_ TIO.putStrLn (sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats x)

-- Parse and Save
main :: IO ()
main = do
  [src] <- getArgs
  articles <- getTimeTitleDescFromSrcWithHash src
  runCoreNLP articles

mkNewsAPIAnalysisDB article =
  NewsAPIAnalysisDB { analysis_sha256 = (A._sha256 article)
                    , analysis_source = (A._source article)
                    , analysis_analysis = ("corenlp" :: T.Text)
                    , analysis_created = (A._created article)
                    }

  
runCoreNLP articles = do

  let dbconfig  = BL.toStrict . BL.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    forM_ (catMaybes articles) $ \(article,(hsh,_,_,x)) -> do
      fchk <- doesFileExist ("/home/modori/data/newsapianalyzed/" ++ (T.unpack hsh))
      when (not fchk) $ do
        eresult <- try $ runCoreNLPParser pp x
        case eresult of
          Left  (e :: SomeException) -> return ()
          Right result               -> do
            saveHashNameBSFileInPrefixSubDirs ("/home/modori/data/newsapianalyzed/" ++ (T.unpack hsh)) (BL.toStrict $ A.encode result)
            uploadAnalysis conn (mkNewsAPIAnalysisDB article)
            
