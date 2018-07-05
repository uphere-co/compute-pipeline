module Main where


import           Control.Lens                   ((^.))
import qualified Options.Applicative       as O
--
import           Pipeline.App.CoreNLPBatch
import           Pipeline.Type                  (configpath,progOption)


main :: IO ()
main = do
  acfg <- O.execParser progOption
  batchCoreNLP (acfg ^. configpath)
