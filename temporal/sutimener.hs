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
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strOption (long "entity" <> short 'e' <> help "Entity File")

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

getSentenceOffsets :: D.Document -> [(Int,(Int,Int))]
getSentenceOffsets doc = 
  let sents = toListOf (D.sentence . traverse) doc
  in zip ([1..] :: [Int]) $ flip map sents $ \s -> 
       let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
           e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
       in (fromIntegral b+1,fromIntegral e)

addText :: Text -> (Int,(Int,Int)) -> (Int,(Int,Int),Text)
addText txt (n,(b,e)) = (n,(b,e),slice (b-1) e txt)

addTag :: [(Int,Int,a)] -> (Int,(Int,Int),Text) -> (Int,(Int,Int),Text,[(Int,Int,a)])
addTag lst (n,(b,e),txt) = (n,(b,e),txt,filter check lst)
  where check (b',e',_) = b' >= b && e' <= e 

addSUTime :: [(Int,(Int,Int),Text)] -> T.ListTimex -> [(Int,(Int,Int),Text,[(Int,Int,Maybe Utf8)])]
addSUTime sents tmxs =
  let f t = ( fromIntegral (t^.T.characterOffsetBegin) + 1
            , fromIntegral (t^.T.characterOffsetEnd)
            , t^. T.timex . Tmx.value
            )
  in filter (not.null.(^._4)) $ map (addTag (map f (tmxs^..T.timexes.traverse))) sents
                     
addNER :: [(Int,(Int,Int),Text)] -> [(Int,Int,a)] -> [(Int,(Int,Int),Text,[(Int,Int,a)])]
addNER sents tags = filter (not.null.(^._4)) $ map (addTag tags) sents

combine :: [(Int,(Int,Int),Text,[a])] -> [(Int,(Int,Int),Text,[b])] -> [(Int,(Int,Int),Text,[a],[b])]
combine sentswithtmx sentswithner = concat $ outer hashing joiner mtmx mner ftmx fner sentswithtmx sentswithner
  where joiner (a1,a2,a3,a4) (_b1,_b2,_b3,b4) = (a1,a2,a3,a4,b4)
        mtmx (a1,a2,a3,a4) = (a1,a2,a3,a4,[])
        mner (b1,b2,b3,b4) = (b1,b2,b3,[],b4)
        ftmx (a1,_a2,_a3,_a4) = a1
        fner (b1,_b2,_b3,_b4) = b1

underlineText :: (Int,Int) -> Text -> [(Int,Int,a)] -> IO ()
underlineText (b0,_e0) txt lst = do
  let f (b,e,_) = ((),b-b0+1,e-b0+1)
      tagged = map f lst
      ann = (AnnotText . map (\(t,m)-> (t,isJust m)) . tagText tagged) txt
      xss = lineSplitAnnot 80 ann
  sequence_ (concatMap (map cutePrintAnnot) xss)

formatResult :: (Int,(Int,Int),T.Text,[(Int,Int,Maybe Utf8)],[(Int,Int,String)]) -> IO ()
formatResult (a1,a2,a3,a4,a5) = do 
  TIO.putStrLn $ "Sentence " <> T.pack (show a1) 
  underlineText a2 a3 a4
  TIO.putStrLn "----------"
  print a4
  TIO.putStrLn "----------"
  print a5
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
      mapM_ formatResult . sortBy (compare `on` view _1) $ combine sentswithtmx sentswithner
      putStrLn "==========================================================="


main :: IO ()
main = do
  pgconn <- PGS.connectPostgreSQL "dbname=nytimes"
  opt <- execParser progOption
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ sort $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True
    pp <- prepare pcfg
    mapM_ (process pgconn pp forest) cnts'
  PGS.close pgconn

