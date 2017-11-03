module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
import           System.Environment                (getArgs)
--
import           Pipeline.Load
import           Pipeline.Util                     (digitsToUTC)

main :: IO ()
main = do
  [t1,t2] <- getArgs
  print ((digitsToUTC t1),(digitsToUTC t2))
  putStrLn "CoreNLP Runner"
