{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.App.CoreNLPBatch where

import           Control.Monad                                (forM,forM_)
import           Data.List.Split                              (chunksOf)
import           System.Process                               (spawnProcess,waitForProcess)
--
import           NewsAPI.Type
--

batchCoreNLP :: IO ()
batchCoreNLP = do
  phs <- forM [("20140101","20141231"),("20150101","20151231"),("20160101","20161231")] $ \(n1,n2) -> do
    spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n1,n2]
  forM_ phs $ \ph -> waitForProcess ph
  
