{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Task.SemanticParser where

import           Control.Concurrent.STM (atomically,modifyTVar',readTVar,writeTVar,retry)
import           Control.DeepSeq      (NFData)
import           Control.Lens         ((&),(.~))
import           Control.Monad        (forever)
import           Data.Aeson           (FromJSON,ToJSON)
import           Data.Binary          (Binary)
import qualified Data.ByteString.Char8 as B
import           Data.Default         (def)
import qualified Data.IntMap     as IM
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Language.Java   as J
import           Language.Java.Inline
import           System.Environment   (getEnv)
--
import           CoreNLP.Simple       (prepare)
import           CoreNLP.Simple.Type  (PipelineConfig(..)
                                      ,tokenizer
                                      ,words2sentences
                                      ,postagger
                                      ,lemma
                                      ,sutime
                                      ,depparse
                                      ,constituency
                                      ,ner
                                      ,isShiftReduce)
import           SRL.Analyze.CoreNLP  (runParser)
import           SRL.Analyze.Type     (DocAnalysisInput)
--
import           CloudHaskell.QueryQueue (QQVar(..),QueryStatus(..),next)


data QCoreNLP = QCoreNLP Text
              deriving (Generic,Show,ToJSON,FromJSON,Binary,NFData)

data RCoreNLP = RCoreNLP DocAnalysisInput
              deriving (Generic,Show,ToJSON,FromJSON,Binary,NFData)

type Pipeline = J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")

daemonCoreNLP :: QQVar QCoreNLP RCoreNLP -> IO ()
daemonCoreNLP qqvar =
  withCoreNLP $ \pp ->
    forever $ do
      (i,q) <- atomically $ do
                 qq <- readTVar qqvar
                 case next qq of
                   Nothing -> retry
                   Just (i,q) -> do
                     let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                     writeTVar qqvar qq'
                     return (i,q)
      case q of
        QCoreNLP txt -> do
          dainput <- runParser pp txt
          let r = RCoreNLP dainput
          atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)


withCoreNLP :: (Pipeline -> IO ()) -> IO ()
withCoreNLP action = do
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
    action pp
