{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Run where

import           Control.Lens                    ((^.),_2,_3,_4)
import           Data.Maybe                   (catMaybes,listToMaybe,mapMaybe)
import           Control.Monad                   (forM,forM_)
import qualified Data.ByteString.Char8  as B
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Data.Text.Read                  (decimal)
import           Language.Java          as J
import           System.Environment              (getEnv)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple.Util
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Convert
import           PropBank
import           WordNet.Type
--
import           Pipeline.Source.NewsAPI.Article
import Pipeline.Source.NYT.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run
import           NLP.Type.PennTreebankII

run :: IO ()
run = do
  clspath <- getEnv "CLASSPATH"
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  db <- loadDB "/data/groups/uphere/data/NLP/dict"
  pdb <- constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  let rdb = constructRoleSetDB pdb
    
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    forM_ (take 10000 filelist) $ \a' -> do
      txt <- getDescription a'
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          tokens = getAllTokens psents
      (_,ppr) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      result <- forM ppr $ \x -> do
        let Right (n,_) = decimal (T.pack (B.unpack $ (x ^. _3)))
        let word = T.pack $ B.unpack $ (x ^. _4)
        let concept = getQueryConcept n (extractPOS $ T.pack $ B.unpack $ (x ^. _2)) db
        return ()
      return ()
  putStrLn "Program is finished!"

runWikiEL = do
  particles <- getAllParsedNYTArticle
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True False False False True)
    forM_ (take 1 particles) $ \pa -> do      
      let txt = T.intercalate "    " pa
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          sents = map (convertSentence pdoc) psents
          tktokss = map (getTKTokens) psents
          tokss = map (mapMaybe convertToken) tktokss
          tokss' = map (mapMaybe convertToken') tktokss
      print tokss
      getWikiEL txt pp >>= print

convertToken' :: TK.Token -> Maybe Token
convertToken' t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)
