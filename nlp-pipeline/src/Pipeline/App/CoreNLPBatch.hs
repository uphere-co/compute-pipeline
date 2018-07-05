{-# LANGUAGE OverloadedStrings   #-}

module Pipeline.App.CoreNLPBatch where

import           Control.Monad                                (forM,forM_)
import           Data.List.Split                              (chunksOf)
import           System.Process                               (spawnProcess,waitForProcess)
--
import           NewsAPI.Type
--

dateRanges = [ [(y++"0101",y++"0430"),(y++"0501",y++"0831"),(y++"0901",y++"1231")] |
               y <- ["2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"] ]

batchCoreNLP :: String -> IO ()
batchCoreNLP fp = do
  forM_ dateRanges $ \dateRange -> do
    phs <- forM dateRange $ \(n1,n2) -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" ["-c",fp,"-b",n1,"-e",n2]
    forM_ phs $ \ph -> waitForProcess ph
