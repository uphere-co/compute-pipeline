{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

--
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type.Simplified
import           WikiEL.EntityLinking
import           WikiEL.WikiNamedEntityTagger

getWikiEL sents emTagger = do
  let mws = map (\(_,_,xs,_) -> xs) sents
      mns = map (\(_,_,_,xs) -> xs) sents
  let unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
      linked_mentions_all = emTagger neTokens
      linked_mentions_resolved
        = filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all
  return linked_mentions_resolved
