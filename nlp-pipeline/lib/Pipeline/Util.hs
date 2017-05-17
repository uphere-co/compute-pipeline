{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


module Pipeline.Util where

import           Control.Lens
import           Control.Monad                    (forM_,join)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Either       (EitherT(runEitherT),hoistEither)
import           Data.Aeson                       (eitherDecodeStrict)
import           Data.Attoparsec.Text             (parseOnly)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Discrimination              (outer)
import           Data.Discrimination.Grouping     (hashing)
import qualified Data.Foldable              as F
import           Data.Function                    (on)
import           Data.List                        (sort,sortBy)
import           Data.Maybe                       (fromJust, isJust)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar               (fromGregorian,Day)
import           Data.Time.Format                 (defaultTimeLocale, formatTime)
import           Data.Time.Clock                  (getCurrentTime,UTCTime(..))
import           Data.Tree
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.Directory.Tree
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.ProtocolBuffers.Basic (Utf8,utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
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
--
import           Intrinio.Type
import           NLP.Type.PennTreebankII
import           System.Console.Haskeline
--
import           YAML.Builder


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

process :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
        -> Forest (Maybe Char)
        -> FilePath
        -> IO ()
process pp forest fp = do
  let sha256 = takeBaseName fp
  -- day <- getArticlePubDay pgconn (B.pack sha256)
  let day = fromGregorian 2099 1 1
  txt <- TIO.readFile fp
  let docu = Document txt day 
  r <- processAnnotation pp forest docu
  case r of
    Left err -> error err
    Right (TaggedResult rsutime rner rdoc) -> do
      print (T._timexes rsutime)
      showHeader fp day
      putStrLn "-----------------------------------------------------------"
      let sentidxs = getSentenceOffsets rdoc
          sents = map (addText txt) sentidxs
          sentswithtmx = addSUTime sents rsutime
          sentswithner = addNER sents rner
      mapM_ formatResult . sortBy (compare `on` view (_1._1)) $ combine sentswithtmx sentswithner
      putStrLn "==========================================================="

run :: IO ()
run = do
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  opt <- execParser progOption
  -- pgconn <- PGS.connectPostgreSQL (B.pack ("dbname=" ++ dbname opt))
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ sort $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True True
    pp <- prepare pcfg
    mapM_ (process pp forest) filelist -- cnts'
  -- PGS.close pgconn

getFileList :: FilePath -> IO ([FilePath])
getFileList fp = do
  list' <- readDirectoryWith return fp
  let filelist = sort . F.toList $ dirTree list'
  return filelist
  
run2 :: IO ()
run2 = do
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  forest <- prepareForest "/data/groups/uphere/F7745.all_entities" -- (entityFile opt)    
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True True
    pp <- prepare pcfg

    forM_ (take 1 filelist) $ \a' -> do
      txt <- getDescription a'
      parseSen txt pp

      let doc = Document txt (fromGregorian 2017 4 17) 
      ann <- annotate pp doc
      (r1, r2) <- processDoc ann
      return ()
      -- print $ filter (\(_,y) -> y /= "U") $ zip (map _token_lemma r2) (map simpleMap $ map _token_pos r2)
      -- process pp forest a'
  putStrLn "Program is finished!"

getDescription f = do
  bstr <- B.readFile f -- "/data/groups/uphere/intrinio/Articles/bloomberg/ffe077729d0ff0ec02fd2b7af537bcf37015171698f99689d96482b2c791c21c"
  let ea = eitherDecodeStrict bstr :: Either String SourceArticles
  case ea of
    Left  _ -> return ""
    Right a -> return (maybe "" id (_description a)) 

simpleMap :: POSTag -> Text
simpleMap p = case p of
  NN   -> "N"
  NNS  -> "N"
  NNP  -> "N"
  NNPS -> "N"
  VB   -> "V"  
  VBZ  -> "V"
  VBP  -> "V"
  VBD  -> "V"
  VBN  -> "V"
  VBG  -> "V"
  JJ   -> "A"
  JJR  -> "A"
  JJS  -> "A"
  RB   -> "R"
  RBR  -> "R"
  RBS  -> "R"
  RP   -> "R"
  _    -> "U"


cutf8' :: Utf8 -> Text
cutf8' = TL.toStrict . TLE.decodeUtf8 . utf8 

convertSentence :: D.Document -> S.Sentence -> Maybe Sentence
convertSentence d s = do
  i <- fromIntegral <$> s^.S.sentenceIndex
  b <- fromIntegral <$> join (firstOf (S.token . traverse . TK.beginChar) s)
  e <- fromIntegral <$> join (lastOf  (S.token . traverse . TK.endChar) s)
  return (Sentence i (b,e) 
            (fromIntegral (s^.S.tokenOffsetBegin),fromIntegral (s^.S.tokenOffsetEnd)))

convertToken :: TK.Token -> Maybe Token
convertToken t = do
  (b',e') <- (,) <$> t^.TK.tokenBeginIndex <*> t^.TK.tokenEndIndex
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8' <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8' <$> (t^.TK.pos)
  l <- cutf8' <$> (t^.TK.lemma)
  return (Token (b,e) w p l)

getSents sents doc =
  let Just newsents = mapM (convertSentence doc) sents
  in newsents

getTokens sents =
  let Just (toklst :: [Token]) = mapM convertToken . concatMap (toListOf (S.token . traverse)) $ sents
  in toklst

processDoc :: J ('Class "edu.stanford.nlp.pipeline.Annotation") -> IO ([Sentence], [Token])
processDoc ann = do
  bstr <- serializeDoc ann
  let lbstr = BL.fromStrict bstr
  case (messageGet lbstr :: Either String (D.Document,BL.ByteString)) of
    Left err -> print err >> return ([],[])
    Right (doc,lbstr') -> do
      let sents = toListOf (D.sentence . traverse) doc
          newsents = getSents sents doc
          toklst = getTokens sents
      return (newsents,toklst)

myaction :: InputT IO (Maybe String)
myaction = do
  str <- getInputLine "Input Sentence : "
  lift (print str)
  return str


-- parseSen :: Text -> Result
parseSen st pp = do
  day <- fmap utctDay getCurrentTime
  let doc = Document st day -- (fromGregorian 2017 4 17)
  ann <- annotate pp doc
  (r1, r2) <- processDoc ann

  return ()

