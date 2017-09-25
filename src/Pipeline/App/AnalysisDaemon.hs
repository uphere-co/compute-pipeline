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
import           Lexicon.Data                      (loadLexDataConfig)
import           NewsAPI.Type
import           NLP.Shared.Type                   (PathConfig,corenlpstore,dbstring,lexconfigpath,mgstore)
import           NLP.Type.CoreNLP
import           RSS                               (rssList)
import           WikiEL.EntityLinking
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.Concurrent
import           Pipeline.Operation.DB
import           Pipeline.Run.CoreNLP
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Type

runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  clspath <- getEnv "CLASSPATH"
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger) <- loadConfig cfgG
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
      forM_ prestigiousNewsSource $ \src -> runCoreNLPforNewsAPISource pp cfg src
      forM_ rssList $ \(src,sec,url) -> runCoreNLPforRSS pp cfg (src ++ "/" ++ sec)
      forM_ prestigiousNewsSource $ \src -> runSRL conn apredata netagger cfg src
      putStrLn "Waiting next run..."
      threadDelay 10000000

  closeConnection conn


coreN = 15 :: Int


-- | This does SRL and generates meaning graphs.
--
runSRL :: PGS.Connection -> AnalyzePredata -> ([Sentence] -> [EntityMention T.Text]) -> PathConfig -> String  -> IO ()
runSRL conn apredata netagger cfg src = do
  as1 <- getAnalysisFilePathBySource cfg src
  as2 <- filterM (\a -> fmap not $ doesFileExist (addExtension ((cfg ^. mgstore) </> a) "mgs")) as1
  loaded1 <- loadCoreNLPResult (map ((</>) (cfg ^. corenlpstore)) as1)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded1
  let (n :: Int) = let n' = ((length loaded) `div` coreN) in if n' >= 1 then n' else 1
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn netagger apredata cfg ls)

  waitForChildren
  refreshChildren

{- 
mkBloombergMGFig :: PathConfig ->  IO ()
mkBloombergMGFig cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger) <- loadConfig cfgG
  forM_ prestigiousNewsSource $ \src -> runSRL conn apredata netagger cfg src
  closeConnection conn
-}
