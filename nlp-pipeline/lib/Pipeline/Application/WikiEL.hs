{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.WikiEL where

import           WikiEL.CoreNLP                        (parseNEROutputStr)
import           WikiEL.WikiEntity                     (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEL.WikiEntityTagger               (buildEntityTable,wikiAnnotator)
import           WikiEL.WikiEntityClass                (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger          (resolveNEs,buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiEL.EntityLinking                  (entityLinking,entityLinkings,buildEntityMentions)

runEL :: IO ()
runEL = do
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    ner_text' = "Billionaire/O investor/O Warren/Person Buffett/PERSON devoted/O a/O substantial/O portion/O of/O his/O letter/O to/O deepen/O his/O long-running/O critique/O of/O investment/O fees/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text')
    tt = getStanfordNEs stanford_nefs
    
  print tt
