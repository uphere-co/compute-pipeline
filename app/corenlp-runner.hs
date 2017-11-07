module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
import           System.Environment                (getArgs)
--
import           Pipeline.App.CoreNLPRunner        (runCoreNLP)
import           Pipeline.Load
import           Pipeline.Type                     (SourceConstraint(..),tempPC)
import           Pipeline.Util                     (digitsToUTC)

main :: IO ()
main = do
  [t1,t2] <- getArgs
  let mbtime = digitsToUTC t1
      metime = digitsToUTC t2
      defaultSC = SourceConstraint Nothing mbtime metime

  runCoreNLP tempPC defaultSC
