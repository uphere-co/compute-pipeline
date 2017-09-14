{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.App.CoreNLPBatch where

import           Control.Exception                            (SomeException,try)
import           Control.Lens
import           Control.Monad                                (forM,forM_,when)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as B
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.Default
import           Data.List.Split                              (chunksOf)
import           Data.Maybe
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import qualified Database.PostgreSQL.Simple            as PGS
import           Language.Java                         as J

import           System.Environment                           (getEnv)
import           System.FilePath                              ((</>))
import           System.Process                               (spawnProcess,waitForProcess)
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           MWE.NamedEntity
import           NewsAPI.DB                                   (uploadAnalysis,uploadArticleError)
import qualified NewsAPI.DB.Article                    as Ar
import           NewsAPI.Type
import           NLP.Type.CoreNLP

import           SRL.Analyze                                  (loadConfig)
import           SRL.Analyze.CoreNLP                          (preRunParser,runParser)
import           WikiEL.EntityLinking
import           WikiEL.Misc
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.Operation.DB
import           Pipeline.Run.WikiEL
import           Pipeline.Type
import           Pipeline.Util

batchCoreNLP :: IO ()
batchCoreNLP = do
  forM_ (chunksOf (length prestigiousNewsSource) prestigiousNewsSource) $ \ns -> do
    phs <- forM ns $ \n -> do
      spawnProcess "./dist/build/corenlp-runner/corenlp-runner" [n]
    forM_ phs $ \ph -> waitForProcess ph
