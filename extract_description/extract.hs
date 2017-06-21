{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad                (filterM)
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict   as HM
import           Data.Function                (on)
import           Data.List                    (foldl',sort,sortBy)
import           Data.Maybe                   (catMaybes,mapMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text.IO          as TIO
import           Language.Java           as J
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           NLP.Type.PennTreebankII
import           NewsAPI.Type (SourceArticles(..))
import           PropBank.Query



printFormat (time,title,desc) = do
  TIO.putStrLn time
  TIO.putStrLn title
  TIO.putStrLn desc
  TIO.putStrLn ""


getTimeTitleDesc :: FilePath -> IO (Maybe (Text,Text,Text))
getTimeTitleDesc fp = do
  bstr <- B.readFile fp 
  let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
  case esrc of
    Left err -> return Nothing
    Right src -> return ((,,) <$> _publishedAt src <*> _title src <*> _description src)


convertToken' :: TK.Token -> Maybe Token
convertToken' t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8' <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8' <$> (t^.TK.pos)
  l <- cutf8' <$> (t^.TK.lemma)
  return (Token (b,e) w p l)


formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


extractVerbs :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> (Text,Text,Text)
             -> IO [Token]
extractVerbs pp (time,title,desc) = do
  -- putStrLn "======"
  -- TIO.putStrLn time  
  -- extractVerbsFromText pp title
  extractVerbsFromText pp desc


extractVerbsFromText :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                     -> Text
                     -> IO [Token]
extractVerbsFromText pp txt = do
  -- TIO.putStrLn txt
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      sents = map (convertSentence pdoc) psents
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken') tktokss
  -- mapM_ print tokss
  return $ concatMap (filter (\t -> isVerb (t^.token_pos))) tokss
  -- mapM_ (mapM_ (putStrLn.formatLemmaPOS)


formatHist :: (Text,Int) -> String
formatHist (txt,n) = printf "%20s   %5d" txt n 


formatPred :: (Text,Text) -> String
formatPred (roleset,definition) = printf "                          %20s : %s" roleset definition

printEachVerb preddb (lemma,n) = do
  putStrLn (formatHist (lemma,n))
  let lst = lookupPredicate preddb lemma
  mapM_ (putStrLn . formatPred) lst 
  putStrLn "---------------------------------------------------------------"


-- doesContainVerb :: Text -> Text -> IO Bool
doesContainVerb pp txt lemma = do 
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      sents = map (convertSentence pdoc) psents
      tktokss = map (getTKTokens) psents
      toks = concatMap (mapMaybe convertToken') tktokss
  -- mapM_ print tokss
  (return . not . null . filter (\t -> isVerb (t^.token_pos) && t^.token_lemma == lemma)) toks
  -- mapM_ (mapM_ (putStrLn.formatLemmaPOS)


main :: IO ()
main = do
  let dir = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg"
  cnts <- getDirectoryContents dir
  let cnts' = (filter (\x -> x /= "." && x /= "..")) cnts
  lst <- flip mapM cnts' $ \fp -> getTimeTitleDesc (dir </> fp)
  let ordered = sortBy (compare `on` (^._1)) $ catMaybes lst 

  let propframedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
  propdb <- constructFrameDB propframedir
  let preddb = constructPredicateDB propdb

 
   
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                  )
    txts <- filterM (\txt -> doesContainVerb pp txt "run") $ map (^._3) ordered
    mapM_ (\t -> TIO.putStrLn t >> TIO.putStrLn "") txts
{-      
    toks <- concat <$> mapM (extractVerbs pp) ordered
    let acc = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty $
                (map (^.token_lemma) toks)
    (mapM_ (printEachVerb preddb) . sortBy (flip compare `on` snd) . HM.toList) acc
-}
