{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.StanfordNER where

import           Data.Text                             (Text)
import qualified Data.Text as T
--
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type.Simplified
import           WikiEL.CoreNLP                        (parseNEROutputStr)
import           WikiEL.WikiEntity                     (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEL.WikiEntityTagger               (buildEntityTable,wikiAnnotator)
import           WikiEL.WikiEntityClass                (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger          (resolveNEs,buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiEL.EntityLinking                  (entityLinking,entityLinkings,buildEntityMentions)
--
import           Pipeline.Util

fromNERSentence (NERSentence xs) = xs
mkTokenToText (txt,ne) = (show txt) ++ "/" ++ (show ne)

getEL txt pp = do
  sents <- getSents' txt pp
  case sents of
    Nothing -> return Nothing
    Just s  -> do
      let ner_texts = map ((T.intercalate " ") . (map T.pack) . (map mkTokenToText) . fromNERSentence . sentToNER) s
      let stanford_nefs =  map (map parseStanfordNE) (map parseNEROutputStr ner_texts)
          tt = map getStanfordNEs stanford_nefs
      return (Just tt)
