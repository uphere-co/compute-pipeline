{-# LANGUAGE OverloadedStrings #-}

module Pipeline.App.RSSMonitor where

import           Control.Applicative        (many)
import           Control.Lens               ((^.))
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.Char                  (isSpace)
import           Data.Either                (isRight)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text            as T
--
import           NER
import           NER.Type
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

tokenizeText = T.split (\c -> (isSpace c) || (c == '\8217'))

printAll cfg = do
  companies <- loadCompanies
  let clist = concat $ map (^. alias) companies 
  forest <- loadForest clist
  items <- fmap (take 10000) $ loadAllRSSItems cfg
  mfitems <- forM items $ \item -> do
    let txts = tokenizeText $ (item ^. description)
        s = runState (runEitherT (many $ pTreeAdvG forest)) txts
    if (isRight (fst s))
      then do
      let Right s' = fst s
      if (length s' > 0)
        then do
{-        print "Matched"
        print (s',item ^. description)
        _ <- getChar -}
        return (Just item)
        else do
{-        print "Not Matched"
        print (item ^. description)
        _ <- getChar -}
        return Nothing
      else return Nothing
  print $ length items
  print $ length (catMaybes mfitems)
