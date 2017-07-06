{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens          hiding (Level)
import           Control.Monad                (filterM)
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict   as HM
import           Data.Function                (on)
import qualified Data.IntMap           as IM
import           Data.List                    (foldl',minimumBy,sort,sortBy,zip4)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import           Data.Tree
import           Language.Java         as J
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           Data.Attribute
import           NewsAPI.Type                                (SourceArticles(..))
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Query
import           SRL.Feature
import           SRL.Feature.Dependency
import           SRL.Feature.Verb
import           SRL.Format                                  (formatBitree,showVerb)
-- import           SRL.Type                                    (Level)
import           SRL.Util
-- import           Text.Format.Tree                            (linePrint)


runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
  
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken') tktokss
  return (psents,sents,tokss,parsetrees,deps)

printFormat (time,title,desc) = do
  T.IO.putStrLn time
  T.IO.putStrLn title
  T.IO.putStrLn desc
  T.IO.putStrLn ""


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
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)


formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))

formatHist :: (Text,Int) -> String
formatHist (txt,n) = printf "%20s   %5d" txt n 


formatPred :: (Text,Text) -> String
formatPred (roleset,definition) = printf "                          %20s : %s" roleset definition


        

printEachVerb preddb (lemma,n) = do
  putStrLn (formatHist (lemma,n))
  let lst = lookupPredicate preddb lemma
  mapM_ (putStrLn . formatPred) lst 
  putStrLn "---------------------------------------------------------------"


extractVerbs :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> (Text,Text,Text)
             -> IO [Token]
extractVerbs pp (time,title,desc) = do
  -- putStrLn "======"
  -- T.IO.putStrLn time  
  -- extractVerbsFromText pp title
  extractVerbsFromText pp desc


extractVerbsFromText :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                     -> Text
                     -> IO [Token]
extractVerbsFromText pp txt = do
  -- T.IO.putStrLn txt
  (_,sents,tokss,_,_) <- runParser pp txt
  -- mapM_ print tokss
  return $ concatMap (filter (\t -> isVerb (t^.token_pos))) tokss
  -- mapM_ (mapM_ (putStrLn.formatLemmaPOS)



doesContainVerb pp txt lemma = do
  (_,sents,tokss,_,_) <- runParser pp txt
  let toks = concat tokss
  (return . not . null . filter (\t -> isVerb (t^.token_pos) && t^.token_lemma == lemma)) toks


verbStatisticsWithPropBank pp preddb lst = do
    toks <- concat <$> mapM (extractVerbs pp) lst
    let acc = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty $
                (map (^.token_lemma) toks)
    (mapM_ (printEachVerb preddb) . sortBy (flip compare `on` snd) . HM.toList) acc


sentStructure pp txt = do
  (psents,sents,tokss,mptrs,deps) <- runParser pp txt
  T.IO.putStrLn txt
  putStrLn "---------------------------------------------------------------"
  mapM_ (putStrLn . formatLemmaPOS) . concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
  putStrLn "---------------------------------------------------------------"
  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,sent,mptr,dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let tkns = zip [0..] (getTKTokens psent)
          tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
      
      let itr = mkAnnotatable (mkPennTreeIdx ptr)
          lmap= mkLemmaMap psent
          iltr = lemmatize lmap itr
          idltr = depLevelTree dep iltr
          vps = verbPropertyFromPennTree lmap ptr
          vtree = verbTree vps idltr 
      mapM_ (T.IO.putStrLn . formatBitree (^._2.to (showVerb tkmap))) vtree
      putStrLn "---------------------------------------------------------------"
      (T.IO.putStrLn . prettyPrint 0) ptr
      putStrLn "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"



    
main :: IO ()
main = do
  let dir = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg"
  cnts <- getDirectoryContents dir
  let cnts' = (filter (\x -> x /= "." && x /= "..")) cnts
  lst <- flip mapM cnts' $ \fp -> getTimeTitleDesc (dir </> fp)
  let ordered = sortBy (compare `on` (^._1)) $ catMaybes lst 

  {- 
  let propframedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
  propdb <- constructFrameDB propframedir
  let preddb = constructPredicateDB propdb
  -}
 
   
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                  )
    -- txts <- filterM (\txt -> doesContainVerb pp txt "run") $ map (^._3) ordered
    -- mapM_ (\t -> T.IO.putStrLn t >> T.IO.putStrLn "") txts
      
    -- verbStatisticsWithPropBank pp preddb ordered
    -- print "hello"
    mapM_ (sentStructure pp . (^._3) ) ordered
     
