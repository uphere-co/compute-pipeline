{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                      ((^.))
import qualified Options.Applicative          as O
--
import           Pipeline.App.CoreNLPRunner        (runCoreNLP)
import           Pipeline.Load
import           Pipeline.Type                     (SourceConstraint(..),btime,configpath',etime,corenlpRunOption)
import           Pipeline.Util                     (digitsToUTC)

main :: IO ()
main = do
  acfg <- O.execParser corenlpRunOption
  cfg <- loadConfigFile (acfg ^. configpath')
  let mbtime = digitsToUTC (acfg ^. btime)
      metime = digitsToUTC (acfg ^. etime)
      defaultSC = SourceConstraint (Just "reuters/Archive") mbtime metime

  runCoreNLP cfg defaultSC
