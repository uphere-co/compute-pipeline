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
import           Network.Transport
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
import           Pipeline.Run
import           Pipeline.Source.NewsAPI.Analysis
import           Pipeline.Source.NewsAPI.Article

nominalDay :: NominalDiffTime
nominalDay = 86400

runDaemon :: IO ()
runDaemon = runSRL

-- | This does SRL and generates meaning graphs.
runSRL :: IO ()
runSRL = do
  as <- getAnalysisFilePathBySource "bloomberg"
  loaded' <- loadCoreNLPResult (map ((</>) "/home/modori/data/newsapianalyzed") as)
  let loaded = catMaybes $ map (\x -> (,) <$> Just (fst x) <*> snd x) loaded'

  (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) <- loadConfig
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  
  let (n :: Int) = ((length loaded) `div` 15)
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks emTagger apredata ls)

  waitForChildren

runCoreNLP :: IO ()
runCoreNLP = do
  forever $ forM_ (chunksOf 3 newsSourceList) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph
    threadDelay 600000000


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
