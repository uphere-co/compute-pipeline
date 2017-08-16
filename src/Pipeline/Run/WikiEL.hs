{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

--
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type.Simplified
import           WikiEL.EntityLinking
import           WikiEL.WikiNamedEntityTagger

prepareNETokens loaded =
  let (all,_,_,_,_,_,_) = loaded
      mws = map (\(_,_,xs,_) -> xs) all
      mns = map (\(_,_,_,xs) -> xs) all
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens

getWikiResolvedMentions loaded emTagger =
  let linked_mentions_all = getWikiAllMentions loaded emTagger
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all

getWikiAllMentions loaded emTagger =
  let neTokens = prepareNETokens loaded
      linked_mentions_all = emTagger neTokens
  in linked_mentions_all
