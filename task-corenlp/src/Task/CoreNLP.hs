{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
--
-- Simple CoreNLP query handler
--
-- Take a sentence and return DocAnalysisInput, which is
-- a forestep for SRL analysis.
--
module Task.CoreNLP where

import           Control.Concurrent.STM ( TMVar )
import           Control.DeepSeq        ( NFData )
import           Control.Exception      ( bracket )
import           Control.Lens           ( (&), (.~) )
import           Data.Aeson             ( FromJSON, ToJSON )
import           Data.Binary            ( Binary )
import qualified Data.ByteString.Char8 as B
import           Data.Default           ( def )
import           Data.Text              ( Text )
import qualified Foreign.JNI as JNI     ( newJVM, destroyJVM )
import           GHC.Generics           ( Generic )
import           Language.Java   as J
import           System.Environment     ( getEnv )
------
import           CoreNLP.Simple         ( prepare)
import           CoreNLP.Simple.Type    ( tokenizer
                                        , words2sentences
                                        , postagger
                                        , lemma
                                        , sutime
                                        , constituency
                                        , ner
                                        )
import           SRL.Analyze.CoreNLP    ( runParser )        -- TODO: this should be located outside SRL.
import           SRL.Analyze.Type       ( DocAnalysisInput )
------
import           CloudHaskell.QueryQueue( type QQVar
                                        , handleQuery
                                        , handleQueryInterrupted
                                        )


data QCoreNLP = QCoreNLP Text
              deriving (Generic,Show,ToJSON,FromJSON,Binary,NFData)

data RCoreNLP = RCoreNLP DocAnalysisInput
              deriving (Generic,Show,ToJSON,FromJSON,Binary,NFData)

type Pipeline = J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")



daemonCoreNLP :: QQVar QCoreNLP RCoreNLP -> IO ()
daemonCoreNLP qqvar =
  withCoreNLP' $ \pp ->
    handleQuery qqvar (\case QCoreNLP txt -> RCoreNLP <$> runParser pp txt)



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

prepareAndProcess :: (Pipeline -> IO ()) -> IO ()
prepareAndProcess action = do
  putStrLn "step1"
  pp <- prepare (def & (tokenizer .~ True)
                     . (words2sentences .~ True)
                     . (postagger .~ True)
                     . (lemma .~ True)
                     . (sutime .~ True)
                     . (constituency .~ True)
                     . (ner .~ True)
                )
  putStrLn "step2"
  action pp

withCoreNLP' :: (Pipeline -> IO ()) -> IO ()
withCoreNLP' action = do
  clspath <- getEnv "CLASSPATH"
  -- jvm <- JNI.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ]
  bracket
    (JNI.newJVM [ B.pack ("-Djava.class.path=" ++ clspath) ])
    (\jvm -> putStrLn "withCoreNLP': finalizer" >> JNI.destroyJVM jvm)
    (\_ -> prepareAndProcess action)
  -- JNI.destroyJVM jvm

queryCoreNLP :: TMVar () -> QQVar QCoreNLP RCoreNLP -> IO ()
queryCoreNLP isDone qqvar =
  prepareAndProcess $ \pp ->
    handleQueryInterrupted isDone qqvar
      (\case QCoreNLP txt -> RCoreNLP <$> runParser pp txt)
