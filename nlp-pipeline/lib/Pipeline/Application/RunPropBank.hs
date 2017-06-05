{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.RunPropBank where

import           Control.Applicative
import           Control.Lens                    ((^.),_1,_2,_3,_4)
import           Control.Monad                   (forM,forM_)
import           Control.Monad.Trans.Either      (EitherT(..))
import qualified Data.ByteString.Char8  as B
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe                      (isNothing)
import           Data.Text                       (Text)
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
import           PM.Type
import           WordNet.Type
import           WordNet.Query                   (WordNetDB)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple                  (annotate,prepare)
import           PropBank

data DB = DB { _wordDB :: WordNetDB
             , _propDB :: RoleSetDB
             , _predDB :: M.Map Text [LinkNet]
             }

getPB = do
  clspath <- getEnv "CLASSPATH"
  flist   <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  db <- getDB
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    forM_ (take 1 flist) $ \f -> runProcess f db pp
  

runPB :: IO ()
runPB = do
  clspath <- getEnv "CLASSPATH"
  flist   <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  db <- getDB
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    result <- forM (take 1 flist) $ \f -> runProcess f db pp
    print result

  putStrLn "Program is finished!"

getDB = do
  worddb  <- loadDB "/data/groups/uphere/data/NLP/dict"
  propdb  <- fmap constructRoleSetDB $ constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  predmat <- loadPM "/data/groups/uphere/data/NLP/PredicateMatrix.v1.3.txt"
  return $ DB worddb propdb predmat

-- runProcess :: 
runProcess f db pp = do
  let worddb  = _wordDB db
      propdb  = _propDB db
      predmat = _predDB db
  txt <- getDescription f    
  doc <- getDoc txt'
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      sents  = convertProtoSents psents pdoc
      tokens = getAllTokens psents
      ukb_input = T.unpack $ mkUkbTextInput (mkUkbInput tokens)
      
  (_,wsdlst) <- getPPR ukb_input 
  
  result <- forM wsdlst $ \w@(wid',wpos',ili',lemma') -> do
    let wid   = T.pack (B.unpack wid')
        wpos  = T.pack (B.unpack wpos')
        ili   = T.pack (B.unpack ili')
        lemma = T.pack (B.unpack lemma')
            
    let Right (n,_) = decimal ili
        concept  :: Maybe ([LexItem],Text) = getQueryConcept n (extractPOS $ wpos) worddb

    print $ getQueryPM ili predmat
        
    (senseSIDofConcept :: [(Text,Maybe Int)]) <- do
      case concept of
        Nothing -> return []
        Just c  -> do
          result <- flip mapM (fst c) $ \c' -> do
            return $ (_lex_word c',getQuerySense (_lex_word c') (_lex_id c') worddb)
          return result
    return senseSIDofConcept


  putStrLn "Individual Sentence"
  putStrLn "-------------------"
  
  mapM_ runSentenceProcess psents

  return $ (txt,result)

runSentenceProcess psent = do
  print $ convertSenToText psent
  let Just tokens = getTokens psent
      ukb_input = T.unpack $ mkUkbTextInput (mkUkbInput tokens)
  (_,wsdlst) <- getPPR ukb_input
  print wsdlst
  

txt' =
  " In a speech from the Rose Garden, Mr. Trump said the landmark 2015 pact imposed wildly \
  \unfair environmental standards on American businesses and workers. He vowed to stand with \
  \the people of the United States against what he called a \"draconian\" international deal. \
  \\"I was elected to represent the citizens of Pittsburgh, not Paris,\" the president said, \
  \drawing support from members of his Republican Party but widespread condemnation from \
  \political leaders, business executives and environmentalists around the globe. \
  \Mr. Trump’s decision to abandon the agreement for environmental action signed by 195 nations \
  \is a remarkable rebuke to heads of state, climate activists, corporate executives and members \
  \of the president's own staff, who all failed to change his mind with an intense, last-minute \
  \lobbying blitz. The Paris agreement was intended to bind the world community into battling \
  \rising temperatures in concert, and the departure of the Earth’s second-largest polluter is a major blow.\" "
