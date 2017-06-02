{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.RunPropBank where

import           Control.Applicative
import           Control.Lens                    ((^.),_1,_2,_3,_4)
import           Control.Monad                   (forM,forM_)
import           Control.Monad.Trans.Either      (EitherT(..))
import qualified Data.ByteString.Char8  as B
import           Data.List
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB   (toLazyText)
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Text.Read                  (decimal)
import           Language.Java          as J
import           System.Environment              (getEnv)
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run
--
import           WordNet.Type
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple                  (annotate,prepare)
import           PropBank

runPB :: IO ()
runPB = do
  clspath <- getEnv "CLASSPATH"

  flist   <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  predmat <- loadPM "/data/groups/uphere/data/NLP/PredicateMatrix.v1.3.txt"
  worddb  <- loadDB "/data/groups/uphere/data/NLP/dict"
  propdb  <- fmap constructRoleSetDB $ constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    forM_ (take 1 flist) $ \f -> do
      txt <- getDescription f
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          sents  = convertProtoSents psents pdoc
          tokens = getTokens psents
          ukb_input = T.unpack $ mkUkbTextInput (mkUkbInput tokens)
      (_,wsdlst) <- getPPR ukb_input 
      
      result <- forM wsdlst $ \w@(wid',wpos',ili',lemma') -> do
        -- runSingleQuery (B.unpack $ (x ^. _3)) (convStrToPOS $ B.unpack $ (x ^. _2)) db
        let wid   = T.pack (B.unpack wid')
            wpos  = T.pack (B.unpack wpos')
            ili   = T.pack (B.unpack ili')
            lemma = T.pack (B.unpack lemma')
            
        let Right (n,_) = decimal ili

        let concept = getQueryConcept n (extractPOS $ wpos) worddb
        let sense = getQuerySense lemma n worddb

        case sense of
          Nothing -> print ""
          Just s  -> print "sense : " >> print s -- print c -- putStrLn (T.unpack c)        
        let (xs,_) = case concept of
              Nothing -> ([],"")
              Just c  -> c -- print c -- putStrLn (T.unpack c)
        flip mapM_ xs $ \x -> do
          print $ T.intercalate "" [_lex_word x,".",T.pack (show $ _lex_id x)]
          queryRoleSet propdb (T.intercalate "" [_lex_word x,".",T.pack (show $ _lex_id x)]) -- (T.pack $ (show $ _lex_word x) ++ "." ++ (show $ _lex_id x))-- input
          case concept of
            Nothing -> print ""
            Just c  -> print c              
        return $ (lemma,fmap (nub . (map (^. _1))) (query ili predmat))
      putStrLn $ show (txt,result)
      -- melr <- getEL txt pp
      -- case melr of
      --   Nothing  -> print "Error in wiki-ner"
      --   Just elr -> print elr 
  putStrLn "Program is finished!"
