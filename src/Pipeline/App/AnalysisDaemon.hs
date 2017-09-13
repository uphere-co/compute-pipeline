{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Distributed.Process
import qualified Control.Distributed.Process as Cloud
import           Control.Distributed.Process.Node
import           Control.Exception
import           Control.Monad           (forever,forM,forM_,void)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Binary             as Bi
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Char8   as B8
import           Data.List.Split              (chunksOf)
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (Text)
import qualified Data.Text               as T
import           Data.Time.Clock                   (NominalDiffTime,UTCTime,addUTCTime,getCurrentTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Network.Transport
import           SRL.Analyze
import           SRL.Analyze.Type                  (AnalyzePredata(..))
import           System.Environment
import           System.FilePath                   ((</>))
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Process
import           Control.Distributed.Process.Node
--
import           Network.Util
import           NewsAPI.Type
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Source.NewsAPI.Article

nominalDay :: NominalDiffTime
nominalDay = 86400

runDaemon :: IO ()
runDaemon = do
  conn <- getConnection "dbname=mydb host=localhost port=65432 user=modori"
  
  -- runCoreNLPAll
  runSRL conn

  closeConnection conn
    
-- | This does SRL and generates meaning graphs.
runSRL :: PGS.Connection -> IO ()
runSRL conn = do
  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats

  as <- getAnalysisFilePathBySource "bloomberg"
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'
  print $ length loaded
  let (n :: Int) = ((length loaded) `div` 1)
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn emTagger apredata ls)

  waitForChildren
  
runCoreNLPAll :: IO ()
runCoreNLPAll = do
  forM_ (chunksOf 3 newsSourceList) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph

runCoreNLP :: String -> IO ()
runCoreNLP src = do
  ph <- spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [src]
  waitForProcess ph
  return ()

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
