{-# LANGUAGE FlexibleContexts #-}

module Pipeline.Run.Idiom
(
  module Generic.SearchTree
, module ParserCustom
, findIdiom
) where

import           Control.Applicative        (many)
import           Control.Monad.State.Lazy   (runState)
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.Tree
--
import           Generic.SearchTree
import           ParserCustom

findIdiom :: (Show a, Ord a) => [a] -> Data.Tree.Forest (Maybe a) -> (Either String [[a]], [a])
findIdiom xs forest = runState (runEitherT (many $ pTreeAdvG forest)) xs
