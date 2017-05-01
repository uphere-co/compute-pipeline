{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Data.Attoparsec.Text             (parseOnly)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                    (toList)
import           Data.Maybe                       (catMaybes, isJust, fromJust)
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
import           Type
import           Util.Doc
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




process_sutime :: PGS.Connection -> J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> FilePath -> IO ()
process_sutime pgconn pp fp = do
  let sha256 = takeBaseName fp
  day <- getArticlePubDay pgconn (B.pack sha256)
  txt <- TIO.readFile fp
  let doc = Document txt day
  ann <- annotate pp doc
  bstr <- serializeTimex ann
  let lbstr = BL.fromStrict bstr
  case (messageGet lbstr :: Either String (T.ListTimex,BL.ByteString)) of
    Left err -> print err
    Right (r,_lbstr') -> do
      let tmxs = toList (r^.T.timexes)
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn $ "date: " ++ formatTime defaultTimeLocale "%F" day
      putStrLn "-----------------------------------------------------------"
      mapM_ (TIO.putStrLn . format) tmxs
      putStrLn "-----------------------------------------------------------"
      let f t = ((),fromIntegral (t^.T.characterOffsetBegin+1), fromIntegral (t^.T.characterOffsetEnd))
          tagged = fmap f tmxs
      let anntxt = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 anntxt
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="

  

process_ner :: Forest Char -> FilePath -> IO ()
process_ner forest fp = do
  txt <- TIO.readFile fp  
  case parseOnly (many (pTreeAdv forest)) txt of
    Left err -> print err
    Right parsed -> do
      putStrLn "==========================================================="
      putStrLn $ "file: " ++ takeFileName fp
      putStrLn "-----------------------------------------------------------"
      let f (b,e,_) = ((),b,e)
          tagged = map f parsed
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
      putStrLn "==========================================================="

process_doc pgconn pp fp= do
  let sha256 = takeBaseName fp
  day <- getArticlePubDay pgconn (B.pack sha256)
  txt <- TIO.readFile fp
  ann <- annotate pp (Document txt day)
  bstr <- serializeDoc ann
  let lbstr = BL.fromStrict bstr
  case (messageGet lbstr :: Either String (D.Document,BL.ByteString)) of
    Left err -> error err
    Right (doc,_) -> do
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
      
      -- print (traverse (D.sentence . S.token) doc)



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
    mapM_ (process_doc pgconn pp) cnts'
    --mapM_ (process_sutime pgconn pp <> process_ner forest) cnts'
  PGS.close pgconn
