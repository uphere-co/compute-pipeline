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
import           OntoNotes.App.Util

runCoreNLPParser :: Text -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                 -> IO ([([Text], [Maybe Token], [Maybe Text], [Maybe Text])],
                        [Maybe Sentence], [SentItem], [[Token]], [Maybe PennTree],
                        [Dependency], Maybe [(SentItem, [(CharIdx, CharIdx, Maybe Text)])])
runCoreNLPParser txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentidxs = getSentenceOffsets psents
      sentitems = map (addText txt) sentidxs
  mtmx' <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> do
      let sentswithtmx = addSUTime sentitems rsutime
      return (Just sentswithtmx)
  let parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  let mtmx = fmap (map (\(x,xs) -> (x,map(\(i,j,z) -> (i,j,(fmap cutf8 z))) xs))) mtmx'
  return ((map convertPsent psents),sents,sentitems,tokss,parsetrees,deps,mtmx)
