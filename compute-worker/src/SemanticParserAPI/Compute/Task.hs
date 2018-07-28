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
-- import           SRL.Analyze.Type                    (ConsoleOutput(..))
--
import           CloudHaskell.Closure                (Capture(..))
import           CloudHaskell.Util                   (ioWorker
                                                     ,spawnChannelLocalDuplex)
--
import           SemanticParserAPI.Compute.Type            (ComputeQuery(..)
                                                           ,ComputeResult(..))
                                                            -- ResultSentence(..)
import           SemanticParserAPI.Compute.Worker          (runSRLQueryDaemon)


sdictBoolBool :: SerializableDict (Bool,Bool)
sdictBoolBool = SerializableDict

sdictInt :: SerializableDict Int
sdictInt = SerializableDict

sdictString :: SerializableDict String
sdictString = SerializableDict

sdictComputeQuery :: SerializableDict ComputeQuery
sdictComputeQuery = SerializableDict

sdictComputeResult :: SerializableDict ComputeResult
sdictComputeResult = SerializableDict

daemonSemanticParser :: (Bool,Bool) -> FilePath -> SendPort ComputeResult -> ReceivePort ComputeQuery -> Process ()
daemonSemanticParser (bypassNER,bypassTEXTNER) lcfg sr rq = do
  -- Semantic Parser worker daemon
  ((sq_i,rr_i),_) <- spawnChannelLocalDuplex $ \(rq_i,sr_i) ->
    ioWorker (rq_i,sr_i) (runSRLQueryDaemon (bypassNER,bypassTEXTNER) lcfg)
  -- Query processing
  forever $ do
    q <- receiveChan rq
    liftIO $ putStrLn ("query received: " ++ show q)
    sendChan sq_i q
    r <- receiveChan rr_i
    -- let r = CR_Sentence (ResultSentence "dummy" [] [] (ConsoleOutput "" "" ""))
    -- liftIO $ putStrLn ("dummy result: " ++ show r)
    sendChan sr r

remotable [ 'sdictBoolBool
          , 'sdictInt
          , 'sdictString
          , 'sdictComputeQuery
          , 'sdictComputeResult
          , 'daemonSemanticParser
          ]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


instance Capture (Bool,Bool) where
  capture = closure (staticDecode $(mkStatic 'sdictBoolBool)) . encode
  staticSdict = $(mkStatic 'sdictBoolBool)


instance Capture Int where
  capture = closure (staticDecode $(mkStatic 'sdictInt)) . encode
  staticSdict = $(mkStatic 'sdictInt)

instance Capture String where
  capture = closure (staticDecode $(mkStatic 'sdictString)) . encode
  staticSdict = $(mkStatic 'sdictString)

instance Capture ComputeQuery where
  capture = closure (staticDecode $(mkStatic 'sdictComputeQuery)) . encode
  staticSdict = $(mkStatic 'sdictComputeQuery)

instance Capture ComputeResult where
  capture = closure (staticDecode $(mkStatic 'sdictComputeResult)) . encode
  staticSdict = $(mkStatic 'sdictComputeResult)



daemonSemanticParser__closure :: Closure ((Bool,Bool) -> FilePath -> SendPort ComputeResult -> ReceivePort ComputeQuery -> Process ())
daemonSemanticParser__closure = staticClosure $(mkStatic 'daemonSemanticParser)
