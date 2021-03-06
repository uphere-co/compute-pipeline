{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Text           ( Text )
import           Options.Applicative ( Parser
                                     , (<**>)
                                     , execParser
                                     , help
                                     , helper
                                     , info
                                     , long
                                     , progDesc
                                     , short
                                     , strOption
                                     )
------
import           CloudHaskell.Util   ( handleError )
------
import           Compute.Worker      ( URL(..), NodeName(..), runWorker )

data WorkerConfig = WorkerConfig { workerConfigOrcURL :: Text
                                 , workerConfigName :: Text
                                 }
                  deriving Show

pOptions :: Parser WorkerConfig
pOptions = WorkerConfig
           <$> strOption ( long "orc"
                        <> short 'o'
                        <> help "URL for orchestrator"
                         )
           <*> strOption ( long "name"
                        <> short 'n'
                        <> help "Name of this node"
                         )


main :: IO ()
main = do
  handleError @String $ do
    cfg <- liftIO $ execParser (info (pOptions <**> helper) (progDesc "worker"))
    let url = URL (workerConfigOrcURL cfg)
        name = NodeName (workerConfigName cfg)
    runWorker url name
