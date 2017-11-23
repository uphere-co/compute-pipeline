{-# LANGUAGE OverloadedStrings #-}

module Pipeline.App.RSSMonitor where

import           Control.Applicative        (many)
import           Control.Lens               ((^.))
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.Either                (isRight)
import           Data.Text                  (Text)
import qualified Data.Text            as T
--
import           NER
import           NLP.Shared.Type           (ItemRSS(..),description,link,pubDate,title)
import           RSS.Load
import           Text.Search.Generic.SearchTree
import           Text.Search.ParserCustom
import           Text.Search.SearchTree


loadCompanies = do
  nt <- loadNameTable 
  companies <- getCompanyList nt
  return companies

loadForest companies = do
  let forest = foldr addTreeItem [] (map T.words companies)
  return forest

printAll cfg = do
  forest <- loadForest =<< loadCompanies
  items <- loadAllRSSItems cfg
  forM_ items $ \item -> do
    let txts = T.words $ (item ^. description)
        s = runState (runEitherT (many $ pTreeAdvG forest)) txts
    when (isRight (fst s)) $ do
      let Right s' = fst s
      when (length s' > 0) $ do
        print s'
