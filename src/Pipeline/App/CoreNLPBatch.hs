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
  forM_ (chunksOf (length prestigiousNewsSource) prestigiousNewsSource) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph
