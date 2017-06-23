{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import           Data.Default
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar                 (fromGregorian)
import           Language.Java         as J
import           System.Environment               (getEnv)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import qualified NLP.Type.NamedEntity    as N
import           WikiEL                           (loadEMtagger)

runWikiEL pp = do
  emTagger <- loadEMtagger "/data/groups/uphere/wikidata/testset/uid"
                           [(N.Org, "/data/groups/uphere/wikidata/testset/ne.org"),
                            (N.Person, "/data/groups/uphere/wikidata/testset/ne.person")]
  txt <- TIO.readFile "test.txt"
  clspath <- getEnv "CLASSPATH"
  let doc = Document txt (fromGregorian 2017 4 17)
  ann <- annotate pp doc
  rdoc <- protobufDoc ann
  case rdoc of
    Left e -> print e
    Right d -> do
      let sents = d ^.. D.sentence . traverse
          f (NERSentence tokens) = tokens
          neTokens =  concatMap (f.sentToNER) sents
          linked_mentions = emTagger neTokens
          text = T.unwords (map fst neTokens)
      mapM_ print linked_mentions
