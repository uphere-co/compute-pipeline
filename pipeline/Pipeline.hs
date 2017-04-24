{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--
import           Type
import           Run
import           System.Directory       (createDirectoryIfMissing,setCurrentDirectory)
import           Options.Applicative

data PipelineOption = PipelineOption { _workingDirectory :: String
                                     , _minorYGPVersion  :: Int
                                     , _minorRSSVersion  :: Int
                                     }

pOptions :: Parser PipelineOption
pOptions = PipelineOption <$> strOption (long "working-directory" <> short 'w' <> help "Working directory")
           <*> option auto (long "minor-YGP-version" <> short 'y' <> help "YGP minor version")
           <*> option auto (long "minor-RSS-version" <> short 'r' <> help "RSS minor version")

pipelineOption = info pOptions ( fullDesc <> progDesc "Pipeline App" <> header "options are working-directory, minor-YGP-version, minor-RSS-version.")

main :: IO ()
main = do
  opt <- execParser pipelineOption
  let wd = _workingDirectory opt
      yv = _minorYGPVersion opt
      rv = _minorRSSVersion opt

  setCurrentDirectory wd

  writeJSON YGP
  writeJSON RSS
  mkRnnApp
  mkCoreNLPApp
  doWikidataAnalysis
  runYGP yv
  runRSS rv
  calculateWI
