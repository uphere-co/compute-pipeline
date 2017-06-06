{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.RunPropBank where

import           Control.Lens                    ((^.))
import           Control.Monad                   (forM)
import qualified Data.ByteString.Char8  as B
import qualified Data.IntMap            as IM
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe                      (fromJust,isNothing)
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Language.Java          as J
--
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Proto.CoreNLPProtos.Sentence
--
import           PM.Type
import           WordNet.Query                   (WordNetDB)
import           CoreNLP.Simple                  (annotate)
import           PropBank

data DB = DB { _wordDB :: WordNetDB
             , _propDB :: RoleSetDB
             , _predDB :: M.Map Text [LinkNet]
             }


getPSents :: Text
          -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> IO [CoreNLP.Proto.CoreNLPProtos.Sentence.Sentence]
getPSents txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  return $ getProtoSents pdoc

getPB :: Text -> DB
      -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
      -> IO [[(Text, Maybe (Text, Text, Maybe [Text]))]]
getPB txt db pp = do
  result <- runProcess txt db pp
  return result

getDB :: IO DB
getDB = do
  worddb  <- loadDB "/data/groups/uphere/data/NLP/dict"
  propdb  <- fmap constructRoleSetDB $ constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  predmat <- loadPM "/data/groups/uphere/data/NLP/PredicateMatrix.v1.3.txt"
  return $ DB worddb propdb predmat

runProcess :: Text -> DB
           -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
           -> IO [[(Text, Maybe (Text, Text, Maybe [Text]))]]
runProcess txt db pp = do
  let predmat = _predDB db
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc      

  result <- mapM (runSentenceProcess predmat) psents

  return result

runSentenceProcess :: M.Map Text [LinkNet]
                   -> CoreNLP.Proto.CoreNLPProtos.Sentence.Sentence
                   -> IO [(Text, Maybe (Text, Text, Maybe [Text]))]
runSentenceProcess predmat psent = do
  
  let Just tokens = getTokens psent
      zt = zip [1..] tokens
      ukb_input = T.unpack $ mkUkbTextInput' (mkUkbInput' zt)
  (_,wsdlst') <- getPPR ukb_input

  let wsdlst = concat $ map (\(_,b,c,d) -> map (\(i,_) -> (B.pack ("w" ++ (show i)),b,c,d)) $ filter (\(_,t) -> (_token_lemma t) == T.pack (B.unpack d)) zt) wsdlst'

  let ordtok = zip [1..] (getTKTokens psent)
      wsd' = map (\(a,_,c,d) -> ((read $ drop 1 (B.unpack a)) :: Int,(T.pack (B.unpack c),T.pack (B.unpack d)))) wsdlst
      k a = if (isNothing (getQueryPM a predmat)) then Nothing else Just (nub $ map (\x -> x ^. propField.lpbRoleset) (fromJust $ getQueryPM a predmat))
      wsd = IM.fromList $ map (\(i,(a,b)) -> (i,(a,b,k a))) wsd'

  result <- forM ordtok $ \(i,t) -> do
    case (IM.lookup i wsd) of
      Nothing -> return (convertTokenToText t,Nothing)
      Just v  -> return (convertTokenToText t,Just v)

  return result
