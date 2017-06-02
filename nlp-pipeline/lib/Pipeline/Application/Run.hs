{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Run where

import           Control.Applicative
import           Control.Lens                    ((^.),_2,_3)
import           Control.Monad                   (forM_)
import           Control.Monad.Trans.Either      (EitherT(..))
import qualified Data.ByteString.Char8  as B
import           Data.List
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as TLB   (toLazyText)
import qualified Data.Text.Lazy.IO      as TLIO
import           Language.Java          as J
import           System.Environment              (getEnv)
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run
--
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple                  (annotate,prepare)
import           YAML.Builder
import           PropBank

findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

run :: IO ()
run = do
  clspath <- getEnv "CLASSPATH"

  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  forest <- prepareForest "/data/groups/uphere/F7745.all_entities"
  pm <- loadPM "/data/groups/uphere/data/NLP/PredicateMatrix.v1.3.txt"
  forestIdiom <- loadIdiom "/data/groups/uphere/data/NLP/idiom.txt"

  db <- loadDB "/data/groups/uphere/data/NLP/dict"
  pdb <- constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  let rdb = constructRoleSetDB pdb
  {-
  let input = "take.01"
  case T.split (== '.') input of
    (x:n:_) -> queryRoleSet rdb input
    (x:[])  -> queryPredicate pdb input
    [] -> putStrLn "query is not recognized."
  -}
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True True True True)
    forM_ (take 100 filelist) $ \a' -> do
      txt <- getDescription a'
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          sents  = convertProtoSents psents pdoc
          tokens = getTokens psents
      -- print $ sents
      -- print $ mkUkbInput tokens
      -- runPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      -- process pp forest a'
      -- TLIO.putStrLn $ TLB.toLazyText (buildYaml 0 (makeYaml 0 tokens))
      getTemporal ann
      -- (_,xs) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))

      -- let (Right s,_) = findIdiom ["such","as","I","live"] forestIdiom
      -- forM_ s $ \x'' -> do
      --   print x''
      
      -- forM_ xs $ \x -> do
      --   runSingleQuery (B.unpack $ (x ^. _3)) (convStrToPOS $ B.unpack $ (x ^. _2)) db
      --   print $ query (T.pack $ B.unpack $ (x ^. _3)) pm

      -- melr <- getEL txt pp
      -- case melr of
      --   Nothing  -> print "Error in wiki-ner"
      --   Just elr -> print elr 
  putStrLn "Program is finished!"
