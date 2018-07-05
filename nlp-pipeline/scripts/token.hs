{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           CoreNLP.Simple.Convert                       (convertPsent)
import           CoreNLP.Simple                               (annotate)
import           Control.Monad                     (forM_,when)
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import           CoreNLP.Simple.Util                          (getDoc,getProtoDoc)
import           CoreNLP.Simple                    (prepare)
import           Control.Lens
import           CoreNLP.Simple.Type               (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           Data.Attoparsec.Text
import           Data.Default                      (def)
import           Data.List                         (isPrefixOf)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text             as T
import           Language.Java         as J
import qualified Data.ByteString.Char8 as B
import           System.Environment                (getEnv)
import           NER.Load
import           NER.Type
import           NLP.Type.CoreNLP
import           Text.Annotation.Util


blackList =
  [ "Merrill Lynch & Co. Inc."
  , "Hennessy Capital Acquisition Corp. III"
  , "Inc. Merrill Lynch & Co."
  , "1-800-Flowers.com, Inc." -- Inconsistency from CoreNLP. 1-800-Flowers . com
  , "1800Flowers.com"
  , "1-800-Flowers.com"
  , "\195\134terna Zentaris Inc."
  , "List of Alphabet Inc. Subsidiaries"
  , "Www.amazon.com"
  , "Http://www.amazon.com"
  , "Amazon.it"
  , "Amazon.in"
  , "Amazon.fr"
  , "Amazon.de"
  , "Amazon.co.UK"
  , "Amazon.co.uk"
  , "Amazon.com/"
  , "Amazon.co.jp"
  , "Amazon.ca"
  , "US Airways\226\128\147American Airlines merger"
  , "AMR Corporation\226\128\147US Airways Group merger"
  , "American Airlines\226\128\147US Airways merger"
  , "Apu.apus.edu"
  ]
  
main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                  )
    companies <- loadCompanies
    let clist = concat $ map (^. alias) companies
--    let clist = ["Google, Inc."]
    forM_ clist $ \c -> do
      when (all (c /=) blackList) $ do
        print c
        doc <- getDoc c -- (T.append c " is a company.")
        ann <- annotate pp doc
        pdoc <- getProtoDoc ann
        let psents = toListOf (D.sentence . traverse) pdoc
            sents = map (convertPsent) psents
            tokens = parseOnly tokenizeText c

--      print sents
        let (ctokens :: [Text]) = concat $ (map (catMaybes . _sentenceWord) sents)
            Right utokens = tokens

        if (utokens `isPrefixOf` ctokens)
          then print "good!"
          else error (show (ctokens,utokens))
