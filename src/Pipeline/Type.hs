
module Pipeline.Type where

import           Options.Applicative
import           Data.Monoid                                ((<>))
import           Data.Text                        (Text)
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
--

type SentIdx = Int
type CharIdx = Int
type BeginEnd = (CharIdx,CharIdx)
type TagPos a = (CharIdx,CharIdx,a)
type SentItem = (SentIdx,BeginEnd,Text)

data ProgOption = ProgOption { dir :: FilePath
                             , entityFile :: FilePath
                             , dbname :: String
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strOption (long "entity" <> short 'e' <> help "Entity File")
                      <*> strOption (long "dbname" <> short 's' <> help "DB name")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "Named Entity Recognition")

data TaggedResult = TaggedResult { resultSUTime :: T.ListTimex
                                 , resultNER :: [(Int,Int,String)]
                                 , resultDoc :: D.Document
                                 }
