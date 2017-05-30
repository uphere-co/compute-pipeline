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
--
import           Generic.SearchTree
import           ParserCustom

findIdiom xs forest = runState (runEitherT (many $ pTreeAdvG forest)) xs
