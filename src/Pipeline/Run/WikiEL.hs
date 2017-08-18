{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

import           Control.Lens
import           Data.Maybe
--
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type.Simplified
import qualified Data.Vector                    as V
import           WikiEL.EntityLinking
import qualified WikiEL.EntityMentionPruning    as EMP
import           WikiEL.WikiNamedEntityTagger

prepareNETokens loaded =
  let (all,_,_,_,_,_,_) = loaded
      mws = all ^.. traverse . sentenceWord
      mns = all ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens

getWikiResolvedMentions loaded emTagger =
  let (_,_,_,tokens,_,_,_) = loaded
      linked_mentions_all = getWikiAllMentions loaded emTagger
      input_pos = V.fromList (map (^. token_pos) $ concat tokens)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered

getWikiAllMentions loaded emTagger =
  let neTokens = prepareNETokens loaded
      linked_mentions_all = emTagger neTokens
  in linked_mentions_all
