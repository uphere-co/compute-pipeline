{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Set     as S
import           Data.List           (foldl')
import qualified Data.Map     as M
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
data POS  = Verb | Noun deriving (Eq,Show,Ord)

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
  


take' n = S.fromList . take n . S.toList
idTriple x = (idPOS x, idPred x, idRole x)
idQuad x = (idPOS x, idPred x, idRole x, mcrIliOffset x)
id4 x = (idPOS x, idPred x, idRole x, pbArg x)
id5 x = (idPOS x, idPred x, idRole x, mcrIliOffset x, pbArg x)
id6 x = (idPOS x, idPred x, idRole x, mcrIliOffset x, pbArg x, pbRoleset x)


main :: IO ()
main = do
  txt <- TIO.readFile "PredicateMatrix.v1.3.txt" 
  let lines = drop 1 $ T.lines txt
      items = map T.words lines

  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat
      enmat100 = take 100 enmat


  {-
  let mm = M.empty

  let dm = foldl' (\acc x -> M.insertWith' (++) (idTriple x) [x] acc) M.empty enmat100
      dm' = fmap length dm

  let dm2 = foldl' (\acc x -> M.insertWith' (++) (idQuad x) [x] acc) M.empty enmat100
      dm2' = fmap length dm2

  let dm3 = foldl' (\acc x -> M.insertWith' (++) (id4 x) [x] acc) M.empty enmat100
      dm3' = fmap length dm3

  let dm4 = foldl' (\acc x -> M.insertWith' (++) (id5 x) [x] acc) M.empty enmat100
      dm4' = fmap length dm4
  
  let dm5 = foldl' (\acc x -> M.insertWith' (++) (id6 x) [x] acc) M.empty enmat100
      dm5' = fmap length dm5

  print dm5'
  -}

  -- print $ length $ S.fromList $ map mcrIliOffset enmat
  -- print $ length $ enmat
  
  -- print dm'
  -- print $ length $ S.fromList $ map (\x-> (idPred x, idRole x, idPOS x)) enmat
  -- print $ length $ enmat

  -- print (take 10 enmat)

  

  {-
  print $ take' 50 $ S.fromList $ map idPred totalmat
  print $ take' 50 $ S.fromList $ map idRole totalmat
  print $ take' 50 $ S.fromList $ map vnClass totalmat
  print $ take' 50 $ S.fromList $ map vnClassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnSubclass totalmat
  print $ take' 50 $ S.fromList $ map vnSubclassNumber totalmat
  print $ take' 50 $ S.fromList $ map vnLema totalmat
  print $ take' 50 $ S.fromList $ map vnRole totalmat
  print $ take' 50 $ S.fromList $ map wnSense totalmat
  print $ take' 50 $ S.fromList $ map mcrIliOffset totalmat
  print $ take' 50 $ S.fromList $ map fnFrame totalmat
  print $ take' 50 $ S.fromList $ map fnLe totalmat
  print $ take' 50 $ S.fromList $ map fnFrameElement totalmat
  print $ take' 50 $ S.fromList $ map pbRoleset totalmat
  print $ take' 50 $ S.fromList $ map pbArg totalmat
  print $ take' 50 $ S.fromList $ map mcrBC totalmat
  print $ take' 50 $ S.fromList $ map mcrDomain totalmat
  print $ take' 50 $ S.fromList $ map mcrSUMO totalmat
  print $ take' 50 $ S.fromList $ map mcrTO totalmat
  print $ take' 50 $ S.fromList $ map mcrLexname totalmat
  print $ take' 50 $ S.fromList $ map mcrBLC totalmat
  print $ take' 50 $ S.fromList $ map wnSensefrec totalmat
  print $ take' 50 $ S.fromList $ map wnSynsetRelNum totalmat
  print $ take' 50 $ S.fromList $ map esoClass totalmat
  print $ take' 50 $ S.fromList $ map esoRole totalmat
  -}
  
  {-
  PRINT $ length totalmat
  print $ length $ filter (\x -> idPOS x == "id:v") totalmat 
  print $ length $ filter (\x -> idPOS x == "id:n") totalmat
  print $ length enmat
  print $ length $ filter (\x -> idPOS x == "id:v") enmat 
  print $ length $ filter (\x -> idPOS x == "id:n") enmat
  -}
  return ()
