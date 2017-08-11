{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

import           Control.Lens
import           Data.Text                          (Text)
import           Data.Time.Calendar                 (fromGregorian)
import           Language.Java                         as J
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           Text.TaggedText
import           WikiEL                           (loadEMtagger)
import           WikiEL.Convert
import           WikiEL.Type.FileFormat
import           WikiEL.WikiEntityClass           (orgClass,personClass)

getWikiEL :: Text -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> IO [TagInfo Text]
getWikiEL txt pp = do
  emTagger <- loadEMtagger (EntityReprFile "/data/groups/uphere/wikidata/testset/uid")
                           [(orgClass, ItemIDFile "/data/groups/uphere/wikidata/testset/ne.org"),
                            (personClass, ItemIDFile "/data/groups/uphere/wikidata/testset/ne.person")]
  let doc = Document txt (fromGregorian 2017 4 17)
  ann <- annotate pp doc
  rdoc <- protobufDoc ann
  case rdoc of
    Left  _ -> return []
    Right d -> do
      let sents = d ^.. D.sentence . traverse
          f (NERSentence tokens) = tokens
          neTokens =  concatMap (f.sentToNER) sents
          linked_mentions = emTagger neTokens
      return $ map (\x -> TagInfo { _taginfo_range = getRangeFromEntityMention x, _taginfo_metainfo = Just $ MetaInfo {_metainfo_info = getNEFromEntityMention x}, _taginfo_text = getNameFromEntityMention x}) linked_mentions
