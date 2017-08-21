{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pipeline.App.CoreNLPRunner where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Maybe
import           Data.Text                                   (Text)
import           Language.Java                         as J
--
import           NLP.Type.PennTreebankII
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           Text.ProtocolBuffers.WireMessage            (messageGet)
--
import           OntoNotes.App.Analyze.CoreNLP               (runParser)
import           OntoNotes.App.Util

runCoreNLPParser = runParser
