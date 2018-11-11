{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Trans.Except (ExceptT(..))
import           Data.Aeson               ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Options.Applicative      ( Parser, (<**>)
                                          , execParser, help, helper
                                          , long, info, progDesc, short
                                          , strOption
                                          )
------
import           CloudHaskell.Type        ( handleError )
import           Worker.Type              ( ComputeConfig(..) )
------
import           Compute.Orchestrator     ( runApp )

data OrcOpt = OrcOpt { computeConfigFile :: FilePath
                     , soFilePath :: FilePath
                     }
            deriving Show

pOptions :: Parser OrcOpt
pOptions = OrcOpt <$> strOption ( long "compute"
                               <> short 'c'
                               <> help "Compute pipeline configuration"
                                )
                  <*> strOption ( long "so"
                               <> short 's'
                               <> help "SO file path"
                                )



main :: IO ()
main = do
  handleError $ do
    opt <- liftIO $ execParser (info (pOptions <**> helper) (progDesc "orchestrator"))
    compcfg :: ComputeConfig <-
      ExceptT $
        eitherDecodeStrict <$> B.readFile (computeConfigFile opt)
    liftIO $ runApp (compcfg,soFilePath opt)
