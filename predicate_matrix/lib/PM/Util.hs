{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module PM.Util where

import           Data.List           (foldl')
import qualified Data.Map     as M
import qualified Data.Set     as S
import Data.Text       (Text)
import qualified Data.Text    as T
--
import           PM.Type


getLang x = case x of
  "id:cat" -> Cat
  "id:eng" -> Eng
  "id:eus" -> Eus
  "id:spa" -> Spa

getPOS x = case x of
  "id:n" -> Noun
  "id:v" -> Verb

getPredicate :: Text -> Predicate
getPredicate txt = last $ T.splitOn ":" txt

getRole :: Text -> Role
getRole txt = last $ T.splitOn ":" txt

getVNClass :: Text -> Text
getVNClass txt = last $ T.splitOn ":" txt

getVNClassNumber :: Text -> Text
getVNClassNumber txt = last $ T.splitOn ":" txt

getVNSubclass :: Text -> Text
getVNSubclass txt = last $ T.splitOn ":" txt

getVnSubclassNumber :: Text -> Text
getVnSubclassNumber txt = last $ T.splitOn ":" txt

getVNLema :: Text -> Text
getVNLema txt = last $ T.splitOn ":" txt

getVNRole :: Text -> Text
getVNRole txt = last $ T.splitOn ":" txt

getWord :: Text -> Text
getWord txt = last $ T.splitOn ":" txt

mkPred x = let {[ idLang' , idPOS' , idPred , idRole
                , vnClass , vnClassNumber , vnSubclass , vnSubclassNumber , vnLema , vnRole
                , wnSense , mcrIliOffset , fnFrame , fnLe , fnFrameElement
                , pbRoleset , pbArg , mcrBC , mcrDomain , mcrSUMO , mcrTO , mcrLexname , mcrBLC
                , wnSensefrec , wnSynsetRelNum , esoClass , esoRole ] = x;
                idLang = getLang idLang';
                idPOS  = getPOS  idPOS';
                }
           in PM {..}



take' n = S.fromList . take n . S.toList
idTriple x = (idPOS x, idPred x, idRole x)
idQuad x = (idPOS x, idPred x, idRole x, mcrIliOffset x)
id4 x = (idPOS x, idPred x, idRole x, pbArg x)
id5 x = (idPOS x, idPred x, idRole x, mcrIliOffset x, pbArg x)
id6 x = (idPOS x, idPred x, idRole x, mcrIliOffset x, pbArg x, pbRoleset x)



createPM :: [PredicateMatrix] -> M.Map Text [(Text,Text)]
createPM mat = fmap (map (\x -> (pbRoleset x, pbArg x))) $ foldl' (\acc x -> M.insertWith' (++) (mcrIliOffset x) [x] acc) M.empty mat

query :: Text -> M.Map Text [(Text,Text)] -> Maybe [(Text,Text)]
query txt pm = M.lookup txt pm
