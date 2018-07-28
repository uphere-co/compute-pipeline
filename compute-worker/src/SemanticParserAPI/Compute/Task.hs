{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module SemanticParserAPI.Compute.Task where

import           Control.Distributed.Process.Closure (remotable,mkStatic)
import           Control.Distributed.Process         (Closure,Process,RemoteTable
                                                     ,SendPort,ReceivePort
                                                     ,sendChan,receiveChan)
import           Control.Distributed.Process.Node    (initRemoteTable)
import           Control.Distributed.Process.Internal.Closure.BuiltIn (staticDecode)
import           Control.Distributed.Process.Serializable  (SerializableDict(..))
import           Control.Distributed.Static          (closure,staticClosure)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Binary                         (encode)
--
import           SRL.Analyze.Type                    (ConsoleOutput(..))
--
import           CloudHaskell.Closure                (Capture(..))
--
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..)
                                                           ,ComputeResult(..)
                                                           ,ResultSentence(..))


sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict

sdictComputeQuery :: SerializableDict ComputeQuery
sdictComputeQuery = SerializableDict

sdictComputeResult :: SerializableDict ComputeResult
sdictComputeResult = SerializableDict

querySemanticParser :: SendPort ComputeResult -> ReceivePort ComputeQuery -> Process ()
querySemanticParser sr rq =
  forever $ do
    q <- receiveChan rq
    liftIO $ putStrLn ("query received: " ++ show q)
    let r = CR_Sentence (ResultSentence "dummy" [] [] (ConsoleOutput "" "" ""))
    liftIO $ putStrLn ("dummy result: " ++ show r)
    sendChan sr r

remotable [ 'querySemanticParser
          , 'sdictInt
          , 'sdictString
          , 'sdictComputeQuery
          , 'sdictComputeResult
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode
  staticSdict = $(mkStatic 'sdictString)

instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode
  staticSdict = $(mkStatic 'sdictInt)

instance Capture ComputeQuery where
  capture = closure (staticDecode $(mkStatic 'sdictComputeQuery)) . encode
  staticSdict = $(mkStatic 'sdictComputeQuery)

instance Capture ComputeResult where
  capture = closure (staticDecode $(mkStatic 'sdictComputeResult)) . encode
  staticSdict = $(mkStatic 'sdictComputeResult)



querySemanticParser__closure :: Closure (SendPort ComputeResult -> ReceivePort ComputeQuery -> Process ())
querySemanticParser__closure = staticClosure $(mkStatic 'querySemanticParser)
