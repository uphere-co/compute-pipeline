{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Monad                     (filterM,forever,forM,forM_)
import           Data.List.Split                   (chunksOf)
import           Data.Maybe                        (catMaybes)
import qualified Data.Text                  as T
import           Data.Time.Clock                   (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as PGS
import           SRL.Analyze                       (loadConfig)
import           SRL.Analyze.Type                  (AnalyzePredata(..))
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>),addExtension)
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Process
--
import           NewsAPI.Type
import           NLP.Type.CoreNLP
import           WikiEL.EntityLinking
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Util

nominalDay :: NominalDiffTime
nominalDay = 86400

batchCoreNLP :: IO ()
batchCoreNLP = do
  forM_ (chunksOf (length prestigiousNewsSource) prestigiousNewsSource) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph

runDaemon :: IO ()
runDaemon = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  forever $ do
    runCoreNLPAll
    forM_ prestigiousNewsSource $ \src -> do
      runSRL conn apredata emTagger src
    putStrLn "Waiting next run..."
    threadDelay 10000000
  closeConnection conn
    
-- | This does SRL and generates meaning graphs.
runSRL :: PGS.Connection -> AnalyzePredata -> ([NERToken] -> [EntityMention T.Text]) -> String -> IO ()
runSRL conn apredata emTagger src = do
  as' <- getAnalysisFilePathBySource src
  as <- filterM (\a -> fmap not $ doesFileExist (addExtension ("/home/modori/temp/mgs" </> a) "mgs")) as'
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  print $ (src,length loaded)
  let (n :: Int) = let n' = ((length loaded) `div` 15) in if n' >= 1 then n' else 1
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn emTagger apredata ls)

  waitForChildren
  refreshChildren
  
runCoreNLPAll :: IO ()
runCoreNLPAll = do
  forM_ (chunksOf (length prestigiousNewsSource) prestigiousNewsSource) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph

runCoreNLP :: String -> IO ()
runCoreNLP src = do
  ph <- spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [src]
  waitForProcess ph
  return ()

refreshChildren = putMVar children []

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
       putMVar children ms
       takeMVar m
       waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())
