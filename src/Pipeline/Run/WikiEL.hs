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

prepareNETokens sents =
  let mws = sents ^.. traverse . sentenceWord
      mns = sents ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens

getWikiResolvedMentions emTagger sents =
  let tokenss = catMaybes <$> sents ^.. traverse . sentenceToken
      linked_mentions_all = getWikiAllMentions emTagger sents
      input_pos = V.fromList (map (^. token_pos) $ concat tokenss)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered

getWikiAllMentions emTagger sents =
  let neTokens = prepareNETokens sents
      linked_mentions_all = emTagger neTokens
  in linked_mentions_all
