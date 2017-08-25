{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


module Pipeline.Util where

import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Either       (EitherT(runEitherT),hoistEither)
import           Data.Attoparsec.Text             (parseOnly)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Discrimination              (outer)
import           Data.Discrimination.Grouping     (hashing)
import           Data.Maybe                       (fromJust, isJust)
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar               (Day)
import           Data.Time.Format                 (defaultTimeLocale, formatTime)
import           Data.Tree
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (createDirectoryIfMissing,withCurrentDirectory)
import           System.FilePath                  ((</>),takeDirectory,takeFileName)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Util
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import 	       	 NLP.Type.CoreNLP
import           Pipeline.Type
import           Text.Annotation.Util.Doc
import           Text.Search.SearchTree
--
import           NLP.Type.PennTreebankII
import           WordNet.Type.POS
import qualified WordNet.Type.POS          as WordNet
import           System.Console.Haskeline
--

processAnnotation :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                  -> Forest (Maybe Char)
                  -> Document
                  -> IO (Either String TaggedResult)
processAnnotation pp forest doc = runEitherT $ do
  ann <- liftIO $ annotate pp doc
  lbstr_sutime <- liftIO $ BL.fromStrict <$> serializeTimex ann
  lbstr_doc    <- liftIO $ BL.fromStrict <$> serializeDoc ann
  TaggedResult <$> (fst <$> hoistEither (messageGet lbstr_sutime))
               <*> hoistEither (parseOnly (many (pTreeAdv forest)) (doc^.doctext))
               <*> (fst <$> hoistEither (messageGet lbstr_doc))

addText :: Text -> (SentIdx,BeginEnd) -> SentItem
addText txt (n,(b,e)) = (n,(b,e),slice (b-1) e txt)

addTag :: [TagPos a] -> SentItem -> (SentItem,[TagPos a])
addTag lst i@(_,(b,e),_) = (i,filter check lst)
  where check (b',e',_) = b' >= b && e' <= e 

addNER :: [SentItem]
       -> [TagPos String]
       -> [(SentItem,[TagPos String])]
addNER sents tags = filter (not.null.(^._2)) $ map (addTag tags) sents

combine :: [(SentItem,[a])] -> [(SentItem,[b])] -> [(SentItem,[a],[b])]
combine lst1 lst2 = concat $ outer hashing joiner m1 m2 f1 f2 lst1 lst2
  where joiner (a1,a2) (_b1,b2) = (a1,a2,b2)
        m1 (a1,a2) = (a1,a2,[])
        m2 (b1,b2) = (b1,[],b2)
        f1 (a1,_) = a1^._1
        f2 (b1,_) = b1^._1

showHeader :: FilePath -> Day -> IO ()
showHeader fp day = do
  putStrLn "==========================================================="
  putStrLn $ "file: " ++ takeFileName fp
  putStrLn $ "date: " ++ formatTime defaultTimeLocale "%F" day

simpleMap :: POSTag -> Maybe WordNet.POS
simpleMap p = case p of
  NN   -> Just POS_N
  NNS  -> Just POS_N
  NNP  -> Just POS_N
  NNPS -> Just POS_N
  VB   -> Just POS_V  
  VBZ  -> Just POS_V
  VBP  -> Just POS_V
  VBD  -> Just POS_V
  VBN  -> Just POS_V
  VBG  -> Just POS_V
  JJ   -> Just POS_A
  JJR  -> Just POS_A
  JJS  -> Just POS_A
  RB   -> Just POS_R
  RBR  -> Just POS_R
  RBS  -> Just POS_R
  RP   -> Just POS_R
  _    -> Nothing

processDoc :: J ('Class "edu.stanford.nlp.pipeline.Annotation") -> IO ([SentenceIndex], [Token])
processDoc ann = do
  pdoc <- getProtoDoc ann
  let sents = getProtoSents pdoc
      newsents = convertProtoSents sents pdoc
      toklst = getAllTokens sents
  return (newsents,toklst)

myaction :: InputT IO (Maybe String)
myaction = do
  sent <- getInputLine "Input Sentence : "
  lift (print sent)
  return sent

mkUkbInput :: [Token] -> [(Text,Maybe WordNet.POS)]
mkUkbInput r2 = filter (\(_,y) -> isJust y) $ zip (map _token_lemma r2) (map simpleMap $ map _token_pos r2)

mkUkbInput' :: [(Int,Token)] -> [(Int,(Text,Maybe WordNet.POS))]
mkUkbInput' r2 = map (\(i,x) -> (i,(_token_lemma x,simpleMap $ _token_pos x))) r2

mkUkbTextInput :: [(Text,Maybe WordNet.POS)] -> Text
mkUkbTextInput r = let jr = map (\(t,mp) -> (t,fromJust mp)) r
                       ptow p = if (p == POS_N) then "#n" else if (p == POS_R) then "#r" else if (p == POS_A) then "#a" else "#v"
                       mkTaggedWord i t p = T.concat [t,ptow p,T.append "#w" (T.pack $ show i),"#1"]
                       rt = T.intercalate " " $ map (\(i,(t,p)) -> mkTaggedWord i t p) (zip [(1 :: Int)..] jr)
                   in rt

mkUkbTextInput' :: [(Int,(Text,Maybe WordNet.POS))] -> Text
mkUkbTextInput' r = let ptow p = if (p == POS_N) then "#n" else if (p == POS_R) then "#r" else if (p == POS_A) then "#a" else "#v"
                        mkTaggedWord i t p = T.concat [t,ptow p,T.append "#w" (T.pack $ show i),"#1"]
                        rt = T.intercalate " " $ map (\(i,(t,mp)) -> mkTaggedWord i t (fromJust mp)) (filter (\(_,(_,mp)) -> isJust mp) r)
                    in rt


getTemporal :: J ('Class "edu.stanford.nlp.pipeline.Annotation") -> IO ()
getTemporal ann = do
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let er = messageGet lbstr_sutime
  case (er :: Either String (T.ListTimex,BL.ByteString)) of
    Left _  -> print ("" :: Text)
    Right r -> print (T._timexes $ fst r)

extractPOS :: Text -> WordNet.POS
extractPOS txt = case (T.last txt) of 
  'n' -> POS_N
  'v' -> POS_V
  'a' -> POS_A
  'r' -> POS_R
  _   -> POS_N

getSents' :: Text
          -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> IO (Maybe [S.Sentence])
getSents' txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  rdoc <- protobufDoc ann
  case rdoc of
    Left  _ -> return Nothing
    Right d -> do
      let sents = d ^.. D.sentence . traverse
      return (Just sents)


saveHashNameBSFileInPrefixSubDirs fp file = do
  let hsh       = takeFileName fp
      storepath = takeDirectory fp
      prefix    = take 2 hsh
      
  withCurrentDirectory storepath $ do
    createDirectoryIfMissing True prefix
    B.writeFile (storepath </> prefix </> hsh) file

saveHashNameTextFileInPrefixSubDirs fp file = do
  let hsh       = takeFileName fp
      storepath = takeDirectory fp
      prefix    = take 2 hsh
      
  withCurrentDirectory storepath $ do
    createDirectoryIfMissing True prefix
    TIO.writeFile (storepath </> prefix </> hsh) file
