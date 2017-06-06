{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module PM.Type where

import Control.Lens
import Control.Lens.TH
import Data.Text        (Text)

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

data Lang = Cat | Eng | Eus | Spa deriving (Eq,Show)
data POS  = Verb | Noun deriving (Eq,Show,Ord)

type Predicate = Text
type Role      = Text

data PropBank = PropBank
  { _lpbRoleset        :: Text -- ^ Predicate in PropBank
  , _lpbArg            :: Text -- ^ Predicate argument in PropBank
  } deriving (Show)

data VerbNet = VerbNet
  { _lvnClass          :: Text -- ^ VerbNet class
  , _lvnClassNumber    :: Text -- ^ VerbNet class number
  , _lvnSubclass       :: Text -- ^ VerbNet subclass
  , _lvnSubclassNumber :: Text -- ^ VerbNet subclass number
  , _lvnLema           :: Text -- ^ Verb lemma
  , _lvnRole           :: Text -- ^ VerbNet thematic-role
  } deriving (Show)

data FrameNet = FrameNet
  { _lfnFrame          :: Text -- ^ Frame in FrameNet
  , _lfnLe             :: Text -- ^ Corresponding lexical-entry in FrameNet
  , _lfnFrameElement   :: Text -- ^ Frame-element in FrameNet
  } deriving (Show)

data ESO = ESO
  { _lesoClass         :: Text -- ^ Class of the ESO ontology
  , _lesoRole          :: Text -- ^ Role of the ESO ontology
  } deriving (Show)

data LinkNet = LinkNet { _propField  :: PropBank
                       , _verbField  :: VerbNet
                       , _frameField :: FrameNet
                       , _esoField   :: ESO
                       } deriving (Show)


makeLenses ''LinkNet
makeLenses ''PropBank
makeLenses ''VerbNet
makeLenses ''FrameNet
makeLenses ''ESO
