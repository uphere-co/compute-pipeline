{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception          (SomeException,try)
import           Control.Lens
import           Control.Monad              (forM_,when)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java              as J
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           MWE
import           NewsAPI.DB                 (uploadAnalysis)
import qualified NewsAPI.DB.Article         as Ar
import           NLP.Type.CoreNLP           (Sentence)
import           OntoNotes.App.Analyze
import           OntoNotes.App.Analyze.SentenceStructure
import           WikiEL.EntityLinking
import           WikiEL.Misc
--
import           Pipeline.App.CoreNLPRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Util
 
-- Load and Run
main'' :: IO ()
main'' = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  fps <- getFileListRecursively "/home/modori/data/newsapianalyzed"
  loaded' <- loadCoreNLPResult fps
  putStrLn "Loading Completed."
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  forM_ loaded $ \(fp,x) -> do
    mapM_ TIO.putStrLn (sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats x)

-- Parse and Save
main' :: IO ()
main' = do
  [src] <- getArgs
  articles <- getTimeTitleDescFromSrcWithHash src
  runCoreNLP articles

main :: IO ()
main = do
  txt <- TIO.readFile "test2.txt"
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  sents <- preRunCoreNLP txt
  let wikiel = getWikiResolvedMentions emTagger sents
      constraint = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ wikiel
  preProcessing sents constraint

preRunCoreNLP :: T.Text -> IO [Sentence]
preRunCoreNLP txt = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (ner .~ True)
                  )
    preRunCoreNLPParser pp txt

runCoreNLP :: [Maybe (Ar.ArticleH,NewsAPIArticleContent)] -> IO ()
runCoreNLP articles = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  
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
            
