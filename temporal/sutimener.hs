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
import           Data.Foldable                    (toList)
import           Data.Maybe                       (catMaybes, fromJust)
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Format                 (defaultTimeLocale, formatTime)
import           Data.Tree
import           Language.Java         as J
import           Options.Applicative
import           System.Directory                 (getDirectoryContents)
import           System.FilePath                  ((</>),takeBaseName,takeExtensions,takeFileName)
import           System.Environment               (getEnv)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
-- import           Type
-- import           Util.Doc
-- import           View
--
import           Annot.NER
import           Annot.SUTime
import           Annot.Util

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
                  -> Forest Char
                  -> Document
                  -> IO (Either String TaggedResult)
processAnnotation pp forest doc = runEitherT $ do
  ann <- liftIO $ annotate pp doc
  lbstr_sutime <- liftIO $ BL.fromStrict <$> serializeTimex ann
  lbstr_doc    <- liftIO $ BL.fromStrict <$> serializeDoc ann
  TaggedResult <$> (fst <$> hoistEither (messageGet lbstr_sutime))
               <*> hoistEither (parseOnly (many (pTreeAdv forest)) (doc^.doctext))
               <*> (fst <$> hoistEither (messageGet lbstr_doc))

showHeader fp day = do
  putStrLn "==========================================================="
  putStrLn $ "file: " ++ takeFileName fp
  putStrLn $ "date: " ++ formatTime defaultTimeLocale "%F" day


showSUTime txt r = do
  let tmxs = toList (r^.T.timexes)
  mapM_ (TIO.putStrLn . format) tmxs
  putStrLn "-----------------------------------------------------------"
  let f t = ((),fromIntegral (t^.T.characterOffsetBegin+1), fromIntegral (t^.T.characterOffsetEnd))
  annotText (fmap f tmxs) txt 

showNER txt parsed = annotText (map (\(b,e,_) -> ((),b,e)) parsed) txt


showDoc doc = do
  let sents = toListOf (D.sentence . traverse) doc
  putStrLn "show number of tokens per each sentence:"
  print $ map (lengthOf (S.token . traverse)) sents 
  putStrLn "show each token"
  mapM_ print $ zip [1..] $ map (catMaybes . toListOf (S.token . traverse . TK.word)) sents
  putStrLn "show each timex"
  mapM_ print $ zip [1..] $ map (catMaybes . toListOf (S.token . traverse . TK.timexValue)) sents
  putStrLn "show starting point"
  mapM_ print $ zip [1..] $ map (fromJust . fromJust . firstOf (S.token . traverse . TK.beginChar)) sents
  putStrLn "show ending point"
  mapM_ print $ zip [1..] $ map (fromJust . fromJust . lastOf (S.token . traverse . TK.endChar)) sents
  putStrLn "show the pair of (starting,ending)"
   -- let  combine :: Lens' s a -> Lens' s b -> Lens' s (a,b)
   --     combine l1 l2 = lens (\s -> (view l1 s, view l2 s)) (\s (x,y) -> set l1 x (set l2 y s))
  mapM_ print $ zip [1..] $ flip map sents $ \s -> 
    let b = fromJust $ fromJust $ firstOf (S.token . traverse . TK.beginChar) s
        e = fromJust $ fromJust $ lastOf  (S.token . traverse . TK.endChar) s
    in (b,e)


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
      showDoc rdoc
      putStrLn "-----------------------------------------------------------"
      showSUTime txt rsutime
      putStrLn "-----------------------------------------------------------"
      showNER txt rner
      putStrLn "==========================================================="


main :: IO ()
main = do
  pgconn <- PGS.connectPostgreSQL "dbname=nytimes"
  opt <- execParser progOption
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True
    pp <- prepare pcfg
    mapM_ (process pgconn pp forest) cnts'
  PGS.close pgconn
