{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Either       (EitherT(runEitherT),hoistEither)
import           Data.Attoparsec.Text             (parseOnly)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Discrimination              (outer)
import           Data.Discrimination.Grouping     (hashing)
import           Data.Function                    (on)
import           Data.List                        (sort,sortBy)
import           Data.Maybe                       (fromJust, isJust)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar               (Day)
import           Data.Time.Format                 (defaultTimeLocale, formatTime)
import           Data.Tree
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.ProtocolBuffers.Basic (Utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           SearchTree
import           Type
import           Util.Doc (slice,tagText)
import           View
--
import           Annot.NER
import           Annot.SUTime

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


processAnnotation :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                  -> Forest (Maybe Char)
                  -> Document
                  -> IO (Either String TaggedResult)
processAnnotation pp forest doc = runEitherT $ do
  ann <- liftIO $ annotate pp doc
  lbstr_sutime <- liftIO $ BL.fromStrict <$> serializeTimex ann
  lbstr_doc    <- liftIO $ BL.fromStrict <$> serializeDoc ann
  TaggedResult <$> (fst <$> hoistEither (messageGet lbstr_sutime))
               <*> hoistEither (parseOnly (many (pTreeAdv forest)) (doc^.doctext))
               <*> (fst <$> hoistEither (messageGet lbstr_doc))

type SentIdx = Int
type CharIdx = Int
type BeginEnd = (CharIdx,CharIdx)
type TagPos a = (CharIdx,CharIdx,a)
type SentItem = (SentIdx,BeginEnd,Text)


getSentenceOffsets :: D.Document -> [(SentIdx,BeginEnd)]
getSentenceOffsets doc = 
  let sents = toListOf (D.sentence . traverse) doc
  in zip ([1..] :: [Int]) $ flip map sents $ \s -> 
       let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
           e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
       in (fromIntegral b+1,fromIntegral e)

addText :: Text -> (SentIdx,BeginEnd) -> SentItem
addText txt (n,(b,e)) = (n,(b,e),slice (b-1) e txt)

addTag :: [TagPos a] -> SentItem -> (SentItem,[TagPos a])
addTag lst i@(_,(b,e),_) = (i,filter check lst)
  where check (b',e',_) = b' >= b && e' <= e 

addSUTime :: [SentItem] -> T.ListTimex
          -> [(SentItem,[TagPos (Maybe Utf8)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
  in filter (not.null.(^._2)) $ map (addTag (map f (tmxs^..T.timexes.traverse))) sents
                     
addNER :: [SentItem]
       -> [TagPos String]
       -> [(SentItem,[TagPos String])]
addNER sents tags = filter (not.null.(^._2)) $ map (addTag tags) sents

combine :: [(SentItem,[a])] -> [(SentItem,[b])] -> [(SentItem,[a],[b])]
combine lst1 lst2 = concat $ outer hashing joiner m1 m2 f1 f2 lst1 lst2
  where joiner (a1,a2) (_b1,b2) = (a1,a2,b2)
        m1 (a1,a2) = (a1,a2,[])
        m2 (b1,b2) = (b1,[],b2)
        f1 (a1,_) = a1^._1
        f2 (b1,_) = b1^._1

underlineText :: BeginEnd -> Text -> [TagPos a] -> IO ()
underlineText (b0,_e0) txt lst = do
  let f (b,e,_) = ((),b-b0+1,e-b0+1)
      tagged = map f lst
      ann = (AnnotText . map (\(t,m)-> (t,isJust m)) . tagText tagged) txt
      xss = lineSplitAnnot 80 ann
  sequence_ (concatMap (map cutePrintAnnot) xss)

formatResult :: (SentItem,[TagPos (Maybe Utf8)],[TagPos String]) -> IO ()
formatResult (s,a,b) = do 
  TIO.putStrLn $ "Sentence " <> T.pack (show (s^._1)) 
  underlineText (s^._2) (s^._3) a
  TIO.putStrLn "----------"
  print a
  TIO.putStrLn "----------"
  print b
  TIO.putStrLn "=========="

showHeader :: FilePath -> Day -> IO ()
showHeader fp day = do
  putStrLn "==========================================================="
  putStrLn $ "file: " ++ takeFileName fp
  putStrLn $ "date: " ++ formatTime defaultTimeLocale "%F" day

process :: PGS.Connection
        -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> Forest (Maybe Char)
        -> FilePath
        -> IO ()
process pgconn pp forest fp= do
  let sha256 = takeBaseName fp
  day <- getArticlePubDay pgconn (B.pack sha256)
  txt <- TIO.readFile fp
  let docu = Document txt day 
  r <- processAnnotation pp forest docu
  case r of
    Left err -> error err
    Right (TaggedResult rsutime rner rdoc) -> do
      showHeader fp day
      putStrLn "-----------------------------------------------------------"
      let sentidxs = getSentenceOffsets rdoc
          sents = map (addText txt) sentidxs
          sentswithtmx = addSUTime sents rsutime
          sentswithner = addNER sents rner
      mapM_ formatResult . sortBy (compare `on` view (_1._1)) $ combine sentswithtmx sentswithner
      putStrLn "==========================================================="


main :: IO ()
main = do
  opt <- execParser progOption
  pgconn <- PGS.connectPostgreSQL (B.pack ("dbname=" ++ dbname opt))
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ sort $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True
    pp <- prepare pcfg
    mapM_ (process pgconn pp forest) cnts'
  PGS.close pgconn

