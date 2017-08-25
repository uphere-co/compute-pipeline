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
import           Data.List                  (find,foldl',groupBy,intersperse)
import           Data.Maybe                 (catMaybes,isJust)
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
  let tb' = find (\t -> (t ^. token_tok_idx_range ^. _1) == b) (concat tokss)
      te' = find (\t -> (t ^. token_tok_idx_range ^. _2) == e) (concat tokss)
  in case (tb',te') of
    (Just tb, Just te) -> Just $ (tb ^. token_char_idx_range ^. _1, te ^. token_char_idx_range ^. _2)
    otherwise          -> Nothing
    
preProcessing = do
  txt <- TIO.readFile "test2.txt"
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
          if ((t ^. token_tok_idx_range) `contained` (head s))
            then do
            if ((t ^. token_tok_idx_range ^. _2) == (snd (head s)))
              then do
              modify' $ (\xs -> drop 1 xs)
              return (t,2)
              else return (t,1)
            else return (t,0)
          else return (t,0)

  let f t1 t2 n1 n2 = case n1 of
        0 -> fillGap t1 t2 n1 n2 " "
        1 -> fillGap t1 t2 n1 n2 "-"
        2 -> fillGap t1 t2 n1 n2 " "
      gapLength t1 t2 = (t2 ^. token_char_idx_range ^. _1) - (t1 ^. token_char_idx_range ^. _2)
      fillGap t1 t2 n1 n2 char = if (n1 == 1 || n1 == 2)
                                 then T.replace "&" "AND" $ T.replace "." "-PERIOD" $ (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char)) 
                                 else (T.append (t1 ^. token_text) (T.replicate (gapLength t1 t2) char))
      result' = (map (\xs -> (map (\((t1,n1),(t2,n2)) -> f t1 t2 n1 n2) (zip xs (drop 1 xs))) ++ [last xs ^. _1 ^. token_text] ) a)
      result = T.intercalate "" $ concat $ intersperse ["\n"] result'

  print tokenss
  TIO.putStrLn result
  print constraint'
  mkNewWikiEL constraint' tokenss >>= print
  


findNETokens con tokss = filter (\t -> (t ^. token_char_idx_range) `contained` con) (concat tokss)
mkNewWikiEL cons tokss = do
  result <- flip evalStateT (0 :: Int) $ do
    forM cons $ \(a,b) -> do
      s <- get
      let ff (a,b) = ((*) 6 $ length $ filter (\t -> '.' `elem` (T.unpack (t ^. token_text))) $ findNETokens (a,b) tokss ) +
                     ((*) 2 $ length $ filter (\t -> '&' `elem` (T.unpack (t ^. token_text))) $ findNETokens (a,b) tokss )
      modify' $ (\s' -> s' + (ff (a,b)))
      return (a + s, b + s + ff (a,b))
  return result

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
            
