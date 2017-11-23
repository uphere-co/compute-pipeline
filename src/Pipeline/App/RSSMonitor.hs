{-# LANGUAGE OverloadedStrings #-}

module Pipeline.App.RSSMonitor where

import           Control.Lens    ((^.))
import           Control.Monad   (forM_)
import qualified Data.Text as T
--
import           NER
import           NLP.Shared.Type (ItemRSS(..),description,link,pubDate,title)
import           RSS.Load
import           Text.Search.Generic.SearchTree
import           Text.Search.SearchTree
--

loadCompanies = do
  nt <- loadNameTable 
  companies <- getCompanyList nt
  return companies

loadForest companies = do
  let forest = foldr addTreeItem [] (map T.words companies)
  return forest

printAll cfg = do
  nt <- loadNameTable 
  companies <- getCompanyList nt

  let forest = foldr addTreeItem [] (map T.words companies)
  print $ searchFunc forest ["Apple"]
  {-
  items <- loadAllRSSItems cfg


  let f i = T.toLower (i ^. description)
      citems = filter (\i -> any (\x -> T.toLower x `T.isInfixOf` (f i)) companies) items

  forM_ citems $ \citem -> do
    print (citem ^. description)
  -}

printAll2 forest = do
  print $ searchFunc forest ["Apple"]
