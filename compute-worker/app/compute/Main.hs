{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..), withExceptT )
import           Data.Text           ( Text )
import qualified Data.Text as T
import           Network.HTTP.Client ( defaultManagerSettings, newManager )
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
import           Servant.Client      ( ClientEnv(..), parseBaseUrl, runClientM )
------
import           CloudHaskell.Type   ( handleError )
------
import           Compute.Worker      ( getCell, getSO, runWorker )

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
    manager' <- liftIO $ newManager defaultManagerSettings
    let url = workerConfigOrcURL cfg
    baseurl <- liftIO $ parseBaseUrl (T.unpack url)
    let env = ClientEnv manager' baseurl Nothing
    cellcfg <-
      withExceptT show $ ExceptT $
        runClientM (getCell (workerConfigName cfg)) env
    so_path <-
      fmap T.unpack $
        withExceptT show $ ExceptT $
          runClientM (getSO) env
    liftIO $ runWorker cellcfg baseurl so_path
