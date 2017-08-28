{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.App.CoreNLPRunner where

import           Control.Exception          (SomeException,try)
import           Control.Lens
import           Control.Monad               (forM_,when)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as B
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Default
import           Data.Maybe
import           Data.Text                                   (Text)
import qualified Data.Text                             as T
import           Language.Java                         as J
import           System.Environment         (getArgs,getEnv)
import           System.Directory           (doesFileExist)

--
import           NewsAPI.DB                 (uploadAnalysis)
import           NLP.Type.CoreNLP           (Sentence)

import qualified NewsAPI.DB.Article         as Ar
import           NLP.Type.PennTreebankII
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Util
import 	       	 NLP.Type.CoreNLP
import           Text.ProtocolBuffers.WireMessage            (messageGet)
--
import           OntoNotes.App.Analyze.CoreNLP               (preRunParser,runParser)
import           OntoNotes.App.Util
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Run
import           Pipeline.Util



runCoreNLPParser = runParser

preRunCoreNLPParser = preRunParser





preRunCoreNLP :: T.Text -> IO [Sentence]
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

runCoreNLP :: [Maybe (Ar.ArticleH,NewsAPIArticleContent)] -> IO ()
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
            
