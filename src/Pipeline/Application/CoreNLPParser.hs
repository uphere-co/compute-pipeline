{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pipeline.Application.CoreNLPParser where

import           Control.Lens          hiding (Level)
import qualified Data.Aeson	       as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import qualified Data.HashMap.Strict   as HM
import           Data.Function                (on)
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap           as IM
import           Data.List                    (foldl',sortBy,zip4)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import           Language.Java         as J
import           System.Environment
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           NLP.Printer.PennTreebankII
import           NLP.Shared.Type
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Query
import           Text.ProtocolBuffers.Basic       (Utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           OntoNotes.App.Util


readAndParse = do
  bstr <- BL.readFile "result.txt"
  let result = A.decode bstr :: Maybe ( [Maybe Sentence], [(SentIdx,BeginEnd,Text)], [[Token]], [Maybe PennTree], [Dependency], Maybe [(SentItem, [TagPos (Maybe Text)])] )
  return result

--runCoreNLPParser :: Text -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
--                 -> IO ( [Maybe Sentence], [(SentIdx,BeginEnd,Text)], [[Token]], [Maybe PennTree], [Dependency], Maybe [(SentItem, [TagPos (Maybe Text)])] )
--                  -> IO ( [S.Sentence], [Maybe Sentence], [(SentIdx,BeginEnd,Text)], [[Token]], [Maybe PennTree], [Dependency], Maybe [(SentItem, [TagPos (Maybe Utf8)])] )
runCoreNLPParser txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentidxs = getSentenceOffsets psents
      sentitems = map (addText txt) sentidxs
  mtmx' <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> do
      let sentswithtmx = addSUTime sentitems rsutime
      return (Just sentswithtmx)
  let parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  let mtmx = fmap (map (\(x,xs) -> map (\(i,j,z) -> (i,j,(fmap cutf8 z))) xs)) mtmx'
  return (sents,sentitems,tokss,parsetrees,deps,mtmx)
--  return (psents,sents,sentitems,tokss,parsetrees,deps,mtmx)

{-
printFormat :: (Text, Text, Text) -> IO ()
printFormat (time,title,desc) = do
  T.IO.putStrLn time
  T.IO.putStrLn title
  T.IO.putStrLn desc
  T.IO.putStrLn ""

convertToken' :: TK.Token -> Maybe Token
convertToken' t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)

formatLemmaPOS :: PrintfType t => Token -> t
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))

formatHist :: (Text,Int) -> String
formatHist (txt,n) = printf "%20s   %5d" txt n 

formatPred :: (Text,Text) -> String
formatPred (roleset,definition) = printf "                          %20s : %s" roleset definition

printEachVerb :: PredicateDB -> (Text, Int) -> IO ()
printEachVerb preddb (lemma',n) = do
  putStrLn (formatHist (lemma',n))
  let lst = lookupPredicate preddb lemma'
  mapM_ (putStrLn . formatPred) lst 
  putStrLn "---------------------------------------------------------------"


extractVerbs :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> (Text,Text,Text)
             -> IO [Token]
extractVerbs pp (_,_,desc) = do
  extractVerbsFromText pp desc


extractVerbsFromText :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                     -> Text
                     -> IO [Token]
extractVerbsFromText pp txt = do
  (_,_,tokss,_,_) <- runParser pp txt
  return $ concatMap (filter (\t -> isVerb (t^.token_pos))) tokss

doesContainVerb :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                -> Text -> Text -> IO Bool
doesContainVerb pp txt lemma' = do
  (_,_,tokss,_,_) <- runParser pp txt
  let toks = concat tokss
  (return . not . null . filter (\t -> isVerb (t^.token_pos) && t^.token_lemma == lemma')) toks


verbStatisticsWithPropBank :: Traversable t => J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                           -> PredicateDB -> t (Text, Text, Text) -> IO ()
verbStatisticsWithPropBank pp preddb lst = do
    toks <- concat <$> mapM (extractVerbs pp) lst
    let acc = foldl' (flip (HM.alter (\case { Nothing -> Just 1; Just n -> Just (n+1)}))) HM.empty $
                (map (^.token_lemma) toks)
    (mapM_ (printEachVerb preddb) . sortBy (flip compare `on` snd) . HM.toList) acc


sentStructure :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
              -> Text -> IO ()
sentStructure pp txt = do
  (psents,sents,tokss,mptrs,deps) <- runParser pp txt
  -- mentions <- getWikiEL txt pp
  T.IO.putStrLn txt
  putStrLn "---------------------------------------------------------------"
  mapM_ (putStrLn . formatLemmaPOS) . concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
  putStrLn "---------------------------------------------------------------"
  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,_,mptr,dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let tkns = zip [0..] (getTKTokens psent)
          tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
          tkmap' = mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns
          
      let itr = mkAnnotatable (mkPennTreeIdx ptr)
          lmap' = mkLemmaMap psent
          iltr = lemmatize lmap' itr
          idltr = depLevelTree dep iltr
          vps = verbPropertyFromPennTree lmap' ptr
          vtree = verbTree vps idltr 
      mapM_ (T.IO.putStrLn . formatBitree (^._2.to (showVerb tkmap))) vtree
      putStrLn "---------------------------------------------------------------"
      showClauseStructureExplicitly lmap' ptr tkmap'
      putStrLn "---------------------------------------------------------------"
      (T.IO.putStrLn . prettyPrint 0) ptr
      -- putStrLn "---------------------------------------------------------------"
      -- print mentions
      putStrLn "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"

formatVerbArgsExplicitly :: VerbArgs (Either (Range,STag) (Int,POSTag)) -> [(Int,Text)] -> PredicateArgument
formatVerbArgsExplicitly va tkmap = predArgs
  where arg0 = case (_va_arg0 va) of
                 Nothing   -> ""
                 Just va''  -> case va'' of
                   Left  ((i,f),_) -> T.intercalate " " $ f' (i,f)
                   Right (n,_)     -> T.intercalate " " $ g' n
                   where f' (i,f) = map (\(_,b) -> b) $ filter (\(a,_) -> a >= i && a <=f) tkmap
                         g' n = map (\(_,b) -> b) $ filter (\(a,_) -> a == n) tkmap
        args = let list = _va_args va
               in flip map list $ \evp -> case evp of
                                            Left  ((i,f),_) -> T.intercalate " " $ f' (i,f)
                                            Right (n,_)     -> T.intercalate " " $ g' n
                                            where f' (i,f) = map (\(_,b) -> b) $ filter (\(a,_) -> a >= i && a <=f) tkmap
                                                  g' n = map (\(_,b) -> b) $ filter (\(a,_) -> a == n) tkmap
        predArgs = PredicateArgument
          { _argument_zero = arg0
          , _predicate = (T.intercalate " " (map (^._2) (va^.va_string)))
          , _arguments = args
          }
          
showClauseStructureExplicitly :: IntMap Lemma -> PennTree -> [(Int,Text)] -> IO ()
showClauseStructureExplicitly lemmamap ptree tkmap' = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
      tr' = bimap (\(_,x)->f x) g (cutOutLevel0 tr)
        where f (S_CL c,l)    = T.pack (show c) <> ":" <> T.pack (show l)
              f (S_SBAR zs,l) = "SBAR:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_VP zs,l)   = "VP:" <> T.pack (show zs) <> "," <> T.pack (show l)
              f (S_PP p,_)    = "PP:" <> T.pack (show p)
              f (S_OTHER p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (S_RT  ,l)    = "ROOT" <> ":" <> T.pack (show l)
              g (Left x)      = T.pack (show x)
              g (Right x)     = T.pack (show x)

  T.IO.putStrLn (formatBitree id tr')

  let rngs = clauseRanges tr
  
  let getVerbArgsLocal vp = do z <- findVerb (vp^.vp_index)  tr
                               return (verbArgs z)

  
  flip mapM_ vps $ \vp -> do
    -- print (findVerb (vp^.vp_index) tr)
    -- print (clauseForVerb rngs vp)
    putStrLn $ printf "%-62s | Clause %7s:  %s"
                 (formatVerbProperty vp)
                 (maybe "" show (clauseForVerb rngs vp))
                 (maybe "" formatVerbArgs (getVerbArgsLocal vp))
    putStrLn $ (maybe "" (show . (flip formatVerbArgsExplicitly tkmap')) (getVerbArgsLocal vp))

getClauseStructureExplicitly :: IntMap Lemma -> PennTree -> [(Int,Text)] -> IO ([Maybe PredicateArgument])
getClauseStructureExplicitly lemmamap ptree tkmap' = do
  let vps  = verbPropertyFromPennTree lemmamap ptree
      tr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptree))
  
  let getVerbArgsLocal vp = do z <- findVerb (vp^.vp_index) tr
                               return (verbArgs z)

  return $ flip map vps $ \vp -> (maybe Nothing (Just . (flip formatVerbArgsExplicitly tkmap')) (getVerbArgsLocal vp))

runConstruction :: IO ()
runConstruction = do
  lst <- getTimeTitleDescFromSrc "bloomberg"
  let ordered = sortBy (compare `on` (^._1)) $ catMaybes lst
   
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )

    mapM_ (sentStructure pp . (^._3) ) ordered

runConstructionWithText :: Text -> IO ()
runConstructionWithText txt  = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )

    result <- getConstruction txt pp
    print result
    -- result2 <- getWikiEL txt pp
    -- print result2

getConstruction :: Text -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")-> IO ([[Maybe PredicateArgument]])
getConstruction txt pp = do
  (psents,sents,_,mptrs,deps) <- runParser pp txt
  flip mapM (zip4 psents sents mptrs deps) $ \(psent,_,mptr,_) -> do
    case mptr of
      Nothing  -> return []
      Just ptr -> do
        let tkns = zip [0..] (getTKTokens psent)
            tkmap' = mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns          
            lmap' = mkLemmaMap psent
        getClauseStructureExplicitly lmap' ptr tkmap'
-}
