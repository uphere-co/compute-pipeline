{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad                     (filterM,forever,forM_)
import qualified Data.ByteString.Char8 as B
import           Data.Default
import           Data.List.Split                   (chunksOf)
import           Data.Maybe                        (catMaybes)
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PGS
import           Language.Java         as J
import           SRL.Analyze                       (loadConfig)
import           SRL.Analyze.Type                  (AnalyzePredata(..))
import           System.Directory                  (doesFileExist)
import           System.Environment                (getEnv)
import           System.FilePath                   ((</>),addExtension)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           Lexicon.Data                           (loadLexDataConfig)
import           NewsAPI.Type
import           NLP.Type.CoreNLP
import           WikiEL.EntityLinking
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.Concurrent
import           Pipeline.Operation.DB
import           Pipeline.Run.CoreNLP
import           Pipeline.Source.NewsAPI.Analysis


runDaemon :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runDaemon corenlpstore errstore mgstore mgdotfigstore arbstore lexconfigpath = do
  clspath <- getEnv "CLASSPATH"
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig lexconfigpath
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig cfgG
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    forever $ do
      forM_ prestigiousNewsSource $ \src -> runCoreNLPforNewsAPISource pp corenlpstore errstore src
      forM_ prestigiousNewsSource $ \src -> runSRL conn apredata emTagger corenlpstore mgstore mgdotfigstore arbstore src
      putStrLn "Waiting next run..."
      threadDelay 10000000

  closeConnection conn


coreN = 15 :: Int

-- | This does SRL and generates meaning graphs.
runSRL :: PGS.Connection -> AnalyzePredata -> ([NERToken] -> [EntityMention T.Text]) -> FilePath -> FilePath -> FilePath -> String -> FilePath -> IO ()
runSRL conn apredata emTagger corenlpstore mgstore mgdotfigstore arbstore src = do
  as' <- getAnalysisFilePathBySource src
  as <- filterM (\a -> fmap not $ doesFileExist (addExtension (mgstore </> a) "mgs")) as'
  loaded' <- loadCoreNLPResult (map ((</>) corenlpstore) as')
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  print $ (src,length loaded)
  let (n :: Int) = let n' = ((length loaded) `div` coreN) in if n' >= 1 then n' else 1
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn emTagger apredata mgstore mgdotfigstore arbstore ls)

  waitForChildren
  refreshChildren

mkBloombergMGFig :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath ->  IO ()
mkBloombergMGFig corenlpstore mgstore mgdotfigstore arbstore lexconfigpath = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig lexconfigpath
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig cfgG
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  runSRL conn apredata emTagger corenlpstore mgstore mgdotfigstore arbstore "bloomberg"
  closeConnection conn
