{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Application.WikiEL where

import           WikiEL.CoreNLP                        (parseNEROutputStr)
import           WikiEL.WikiNamedEntityTagger          (getStanfordNEs,parseStanfordNE)

runEL :: IO ()
runEL = do
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    tt = getStanfordNEs stanford_nefs
    
  print tt
