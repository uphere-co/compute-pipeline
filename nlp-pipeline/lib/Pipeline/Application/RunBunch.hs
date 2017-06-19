{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Pipeline.Application.RunBunch where

import           Control.Applicative             ((<*>))
import           Control.Lens                    ((^.))
import           Control.Monad                   (forM,forM_)
import           Data.Aeson                      (FromJSON,ToJSON)
import           Data.Aeson.Types                (fieldLabelModifier)
import qualified Data.Aeson             as A
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.IntMap            as IM
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe                      (fromJust,isNothing)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Data.Text.Read                  (decimal)
import           GHC.Generics
import           Language.Java          as J
import           Options.Applicative
import           System.Environment              (getEnv)
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Proto.CoreNLPProtos.Sentence
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

data NLPPOption = NLPPOption { _textPath       :: FilePath
                             , _dbWordNetPath  :: FilePath
                             , _dbPropBankPath :: FilePath
                             , _dbPredMatPath  :: FilePath
                             } deriving (Generic,Show)

instance ToJSON NLPPOption where
  toJSON = A.genericToJSON A.defaultOptions { fieldLabelModifier = drop 1 }
    
instance FromJSON NLPPOption where
  parseJSON = A.genericParseJSON A.defaultOptions { fieldLabelModifier = drop 1 }

pOptions :: NLPPOption -> Parser NLPPOption
pOptions def = NLPPOption <$> strOption (long "text" <> short 't' <> value (_textPath def) <> help "Path storing text files")
                          <*> strOption (long "word" <> short 'w' <> value (_dbWordNetPath def) <> help "WordNet DB Path")
                          <*> strOption (long "prop" <> short 'p' <> value (_dbPropBankPath def) <> help "PropBank DB Path")
                          <*> strOption (long "pred" <> short 'm' <> value (_dbPredMatPath def) <> help "Predicate Matrix DB Path")

nlppOption :: NLPPOption -> ParserInfo NLPPOption
nlppOption def = info (pOptions def) (fullDesc <> progDesc "NLP-Pipeline")

getPSents :: Text
          -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> IO [CoreNLP.Proto.CoreNLPProtos.Sentence.Sentence]
getPSents txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  return $ getProtoSents pdoc

getPB :: FilePath
      -> DB
      -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
      -> IO [(Text, [[(Text, Maybe Int)]])]
getPB fp db pp = do
  flist <- getFileList fp
  result <- forM (take 1 flist) $ \f -> runProcess f db pp
  return result

loadConfig fp = do
  bt <- BL.readFile fp
  return bt
  
runPB :: IO ()
runPB = do
  clspath <- getEnv "CLASSPATH"
  config' <- loadConfig "config/config.json"
  
  let (def :: NLPPOption) = fromJust $ A.decode config'

  opt <- execParser (nlppOption def)
  
  let tfp = _textPath opt
      wdb = _dbWordNetPath opt
      pdb = _dbPropBankPath opt
      mdb = _dbPredMatPath opt
  
  flist   <- getFileList tfp
  db <- getDB (wdb,pdb,mdb)
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    forM_ (take 10000 flist) $ \f -> runProcess f db pp
  putStrLn "Program is finished!"

getDB :: (FilePath,FilePath,FilePath) -> IO DB
getDB (wdb,pdb,mdb) = do
  worddb  <- loadDB wdb
  propdb  <- fmap constructRoleSetDB $ constructPredicateDB <$> constructFrameDB pdb
  predmat <- loadPM mdb
  return $ DB worddb propdb predmat

runProcess :: FilePath
           -> DB
           -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
           -> IO (Text, [[(Text, Maybe Int)]])
runProcess f db pp = do
  let worddb  = _wordDB db
      predmat = _predDB db
  txt <- getDescription f    
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      tokens = getAllTokens psents
      ukb_input = T.unpack $ mkUkbTextInput (mkUkbInput tokens)
      
  (_,wsdlst) <- getPPR ukb_input 
  
  result <- forM wsdlst $ \(_,wpos',ili',_) -> do
    let -- wid   = T.pack (B.unpack wid')
        wpos  = T.pack (B.unpack wpos')
        ili   = T.pack (B.unpack ili')
        -- lemma = T.pack (B.unpack lemma')
            
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
  
  mapM_ (runSentenceProcess predmat) psents

  return $ (txt,result)


runSentenceProcess :: M.Map Text [LinkNet]
                   -> CoreNLP.Proto.CoreNLPProtos.Sentence.Sentence
                   -> IO ()
runSentenceProcess predmat psent = do
  return $ convertSenToText psent
  
  let Just tokens = getTokens psent
      zt = zip [1..] tokens
      ukb_input = T.unpack $ mkUkbTextInput' (mkUkbInput' zt)
  (_,wsdlst') <- getPPR ukb_input


  let wsdlst = concat $ map (\(_,b,c,d) -> map (\(i,_) -> (B.pack ("w" ++ (show i)),b,c,d)) $ filter (\(_,t) -> (_token_lemma t) == T.pack (B.unpack d)) zt) wsdlst'

  print $ convertSenToText psent
  print ukb_input
  
  let ordtok = zip [1..] (getTKTokens psent)
      wsd' = map (\(a,_,c,d) -> ((read $ drop 1 (B.unpack a)) :: Int,(T.pack (B.unpack c),T.pack (B.unpack d)))) wsdlst
      k a = if (isNothing (getQueryPM a predmat)) then Nothing else Just (nub $ map (\x -> x ^. propField.lpbRoleset) (fromJust $ getQueryPM a predmat))
      wsd = IM.fromList $ map (\(i,(a,b)) -> (i,(a,b,k a))) wsd'

  pred' <- forM ordtok $ \(i,t) -> do
    case (IM.lookup i wsd) of
      Nothing -> return (convertTokenToText t,Nothing)
      Just v  -> return (convertTokenToText t,Just v)

  print pred'
