{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.WikiEL where

import           Control.Lens
import           Data.Maybe
--
import           CoreNLP.Simple.Convert
import           Data.Text                             (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T.IO
import qualified Data.Vector                    as V
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity
import           System.FilePath                          ((</>))
import           WikiEL.EntityLinking
import qualified WikiEL.EntityMentionPruning    as EMP
import           WikiEL.Misc
import           WikiEL.WikiNamedEntityTagger
--
import           WikiEL.Type.FileFormat
import qualified WikiEL.WikiEntityClass        as WC
import qualified WikiEL.WikiNamedEntityTagger  as WNET
import qualified WikiEL                        as WEL

prepareNETokens :: [Sentence] -> [NERToken]
prepareNETokens sents =
  let mws = sents ^.. traverse . sentenceWord
      mns = sents ^.. traverse . sentenceNER
      unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns)
  in neTokens

prepareWNP sents =
  let ws = catMaybes $ sents ^.. traverse . sentenceWord . traverse
      ns' = catMaybes $ sents ^.. traverse . sentenceNER . traverse 
      ns = map (\x -> (fromMaybe (error (show x)) . classify) x) ns'
      ts = catMaybes $ sents ^.. traverse . sentenceToken . traverse
      ps = map (^. token_pos) ts
  in (zip3 ws ns ps)

getWikiResolvedMentions :: ([NERToken] -> [EntityMention w]) -> [Sentence] -> [EntityMention w]
getWikiResolvedMentions emTagger sents =
  let tokenss = catMaybes <$> sents ^.. traverse . sentenceToken
      linked_mentions_all = getWikiAllMentions emTagger sents
      input_pos = V.fromList (map (^. token_pos) $ concat tokenss)
      linked_mentions_all_unfiltered = (EMP.filterEMbyPOS input_pos linked_mentions_all)
  in filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all_unfiltered


getWikiAllMentions :: ([NERToken] -> [EntityMention w]) -> [Sentence] -> [EntityMention w]
getWikiAllMentions emTagger sents =
  let neTokens = prepareNETokens sents
      linked_mentions_all = emTagger neTokens
  in linked_mentions_all

mkConstraintFromWikiEL :: [EntityMention Text] -> [(Int,Int)]
mkConstraintFromWikiEL wikiel = map (\x -> let irange = entityIRange x in (beg irange, end irange)) $ wikiel

runEL sents (tagger,entityResolve) = do
  let
    wnps = prepareWNP sents
    linked_mentions = tagger wnps
    disambiguated_mentions = entityResolve linked_mentions

  mapM_ print disambiguated_mentions


loadWikiData sents = do
  edges  <- WEL.loadAndSortEdges graphFilesG
  uidTag <- WEL.loadFiles classFilesG
  titles <- WEL.loadWikipageMapping wikiTitleMappingFileG
  tagger <- WEL.loadFEMtagger reprFileG classFilesG
  entityResolve = WEL.disambiguateMentions edges uidTag titles


  runEL sents (tagger,entityResolve)

reprFileTinyG       = EntityReprFile (globalData </> "wiki-ner/data/wikidata.test.entities")
orgItemFileG        = ItemIDFile (globalData </> "wiki-ner/data/ne.org")
personItemFileG     = ItemIDFile (globalData </> "wiki-ner/data/ne.person")
brandItemFileG      = ItemIDFile (globalData </> "wiki-ner/data/ne.brand")
locationItemFileG   = ItemIDFile (globalData </> "wiki-ner/data/ne.loc")
occupationItemFileG = ItemIDFile (globalData </> "wiki-ner/data/ne.occupation")
humanRuleItemFileG  = ItemIDFile (globalData </> "wiki-ner/data/ne.human_rule")
buildingItemFileG   = ItemIDFile (globalData </> "wiki-ner/data/ne.building")

classFilesG = [ (WC.personClass, personItemFileG)
              , (WC.orgClass,    orgItemFileG)
              , (WC.brandClass,  brandItemFileG)
              , (WC.occupationClass, occupationItemFileG)
              , (WC.locationClass,   locationItemFileG)
              , (WC.humanRuleClass,  humanRuleItemFileG)
              , (WC.buildingClass,   buildingItemFileG)
              ]

reprFileG     = EntityReprFile (globalData </> "wiki-ner/data/names")

-- Full data
wikiTitleMappingFileG = WikiTitleMappingFile (globalData </> "wiki-ner/wiki_id.page_title.txt")
wordnetMappingFileG   = WordNetMappingFile (globalData </> "wiki-ner/page_id.wiki_id.wordnet.tsv")
propertyNameFileG     = PropertyNameFile (globalData </> "wiki-ner/properties.tsv")
listedCompanyFileG    = globalData </> "enwiki/companies"

graphFilesG = globalData </> "wiki-ner/interlinks.filtered"

globalData = "/data/groups/uphere/data/Wiki"

