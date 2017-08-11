{-# LANGUAGE FlexibleContexts #-}

module Pipeline.Run.Idiom
(
) where

import           Control.Applicative        (many)
import           Control.Monad.State.Lazy   (runState)
import           Control.Monad.Trans.Either (EitherT(..))
import           Data.Tree
--
