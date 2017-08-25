{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception          (SomeException,try)
import           Control.Lens
import           Control.Monad              (forM_,replicateM_,void,when)
import           Control.Monad.State
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List                  (find,foldl',groupBy)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Language.Java              as J
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs,getEnv)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           NewsAPI.DB                 (uploadAnalysis)
import qualified NewsAPI.DB.Article         as A
import           NewsAPI.Type               (NewsAPIAnalysisDB(..))
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

contained (a,b) (c,d) = if (a >= c && b <=d) then True else False
isContained (a,b) xs = foldl' (\acc x -> if ((a,b) `contained` x) then (acc || True) else (acc || False)) False xs

-- Parse and Save
main' :: IO ()
main' = do
  [src] <- getArgs
  articles <- getTimeTitleDescFromSrcWithHash src
  runCoreNLP articles

main :: IO ()
main = preProcessing


convertConstraintInCharIdx (b,e) tokss =
  let tb' = find (\t -> (fst $ _token_tok_idx_range t) == b) (concat tokss)
      te' = find (\t -> (snd $ _token_tok_idx_range t) == e) (concat tokss)
  in case (tb',te') of
    (Just tb, Just te) -> Just $ (fst $ _token_char_idx_range tb, snd $ _token_char_idx_range te)
    otherwise          -> Nothing
    
preProcessing = do
  txt <- TIO.readFile "test.txt"
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  sents <- preRunCoreNLP txt
  let tokenss = catMaybes <$> sents ^.. traverse . sentenceToken

  let constraint = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ getWikiResolvedMentions emTagger sents
      constraint' = catMaybes $ map (\c -> convertConstraintInCharIdx c tokenss) constraint
  a <- flip evalStateT constraint $ do
    forM tokenss $ \tokens -> do
      forM tokens $ \t -> do
        s <- get
        if (not $ null s)
          then do
          if (_token_tok_idx_range t `contained` (head s))
            then do
            if ((snd $ _token_tok_idx_range t) == (snd (head s)))
              then do
              modify' $ (\xs -> drop 1 xs)
              return (t,2)
              else return (t,1)
            else return (t,0)
          else return (t,0)

  let f t1 t2 n1 n2 = case n1 of
        0 -> fillGap t1 t2 " "
        1 -> fillGap t1 t2 "-"
        2 -> fillGap t1 t2 " "
      gapLength t1 t2 = (fst $ _token_char_idx_range t2) - (snd $ _token_char_idx_range t1)
      fillGap t1 t2 char = (T.append (_token_text t1) (T.replicate (gapLength t1 t2) char)) 
      result' = (map (\xs -> (map (\((t1,n1),(t2,n2)) -> f t1 t2 n1 n2) (zip xs (drop 1 xs))) ++ [(_token_text $ fst $ last xs)] ) a)
      result = T.intercalate "" $ concat result'

  TIO.putStrLn result
  print constraint'
  print tokenss
  
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
            
