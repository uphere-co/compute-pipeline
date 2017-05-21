{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Set     as S
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

data PredicateMatrix = PM
  { idLang           :: Lang -- ^ Language of the predicate
  , idPOS            :: POS  -- ^ Part-of-speech of the predicate
  , idPred           :: Predicate -- ^ Predicate  
  , idRole           :: Role -- ^ Role
  , vnClass          :: Text -- ^ VerbNet class
  , vnClassNumber    :: Text -- ^ VerbNet class number
  , vnSubclass       :: Text -- ^ VerbNet subclass
  , vnSubclassNumber :: Text -- ^ VerbNet subclass number
  , vnLema           :: Text -- ^ Verb lemma
  , vnRole           :: Text -- ^ VerbNet thematic-role
  , wnSense          :: Text -- ^ Word sense in WordNet
  , mcrIliOffset     :: Text -- ^ ILI number in the MCR3.0
  , fnFrame          :: Text -- ^ Frame in FrameNet
  , fnLe             :: Text -- ^ Corresponding lexical-entry in FrameNet
  , fnFrameElement   :: Text -- ^ Frame-element in FrameNet
  , pbRoleset        :: Text -- ^ Predicate in PropBank
  , pbArg            :: Text -- ^ Predicate argument in PropBank
  , mcrBC            :: Text -- ^ Whether the verb sense is Base Concept
                             --   or not in the MCR3.0
  , mcrDomain        :: Text -- ^ WordNet domain aligned to WordNet 3.0 in the MCR3.0
  , mcrSUMO          :: Text -- ^ AdimenSUMO in the MCR3.0
  , mcrTO            :: Text -- ^ MCR Top Ontology in the MCR3.0
  , mcrLexname       :: Text -- ^ MCR Lexicographical file name
  , mcrBLC           :: Text -- ^ Base Level Concept of the WordNet verb sense in the MCR3.0
  , wnSensefrec      :: Text -- ^ Frequency of the WordNet 3.0 verb sense
  , wnSynsetRelNum   :: Text -- ^ Number of relations of the WordNet 3.0 verb sense
  , esoClass         :: Text -- ^ Class of the ESO ontology
  , esoRole          :: Text -- ^ Role of the ESO ontology
  } deriving (Show)

mkPred x = let {[ idLang' , idPOS' , idPred , idRole
                , vnClass , vnClassNumber , vnSubclass , vnSubclassNumber , vnLema , vnRole
                , wnSense , mcrIliOffset , fnFrame , fnLe , fnFrameElement
                , pbRoleset , pbArg , mcrBC , mcrDomain , mcrSUMO , mcrTO , mcrLexname , mcrBLC
                , wnSensefrec , wnSynsetRelNum , esoClass , esoRole ] = x;
                idLang = getLang idLang';
                idPOS  = getPOS  idPOS';
                }
           in PM {..}

checkData txtss = do
  let len = map length txtss
  print len

data Lang = Cat | Eng | Eus | Spa deriving (Eq,Show)
data POS  = Verb | Noun deriving (Eq,Show)

type Predicate = Text
type Role      = Text


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

getVNClass txt = last $ T.splitOn ":" txt
getVNClassNumber txt = last $ T.splitOn ":" txt
getVNSubclass txt = last $ T.splitOn ":" txt
getVNSubclassNumber txt = last $ T.splitOn ":" txt
getVNLema txt = last $ T.splitOn ":" txt
getVNRole txt = last $ T.splitOn ":" txt
getWord txt = last $ T.splitOn ":" txt

take' n = S.fromList . take n . S.toList

main :: IO ()
main = do
  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines
  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat
  print $ S.fromList $ map idRole totalmat
  print (take 10 enmat)
  return ()
