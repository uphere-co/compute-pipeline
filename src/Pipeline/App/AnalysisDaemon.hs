{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Monad                     (forM,forM_)
import           Data.List.Split                   (chunksOf)
import           Data.Maybe                        (catMaybes)
import           Data.Time.Clock                   (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as PGS
import           SRL.Analyze                       (loadConfig)
import           SRL.Analyze.Type                  (AnalyzePredata(..))
import           System.FilePath                   ((</>))
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Process
--
import           NewsAPI.Type
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Source.NewsAPI.Analysis


nominalDay :: NominalDiffTime
nominalDay = 86400

runDaemon :: IO ()
runDaemon = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  
  runCoreNLPAll
  
  forM_ prestigiousNewsSource $ \src -> do
    runSRL conn src

  closeConnection conn
    
-- | This does SRL and generates meaning graphs.
runSRL :: PGS.Connection -> String -> IO ()
runSRL conn src = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats

  as <- getAnalysisFilePathBySource src
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  print $ length loaded
  let (n :: Int) = ((length loaded) `div` 15)
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn emTagger apredata ls)

  waitForChildren
  refreshChildren
  
runCoreNLPAll :: IO ()
runCoreNLPAll = do
  forM_ (chunksOf 3 prestigiousNewsSource) $ \ns -> do
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
