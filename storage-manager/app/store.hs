{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Aeson                 (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup             ((<>))
import           Options.Applicative        (Parser,execParser,fullDesc,help,info,long,progDesc,short,strOption)
--
import           Storage.Config             (StorageConfig)


data StoreOption = StoreOption {
                     storeConfig :: FilePath
                     }
                 deriving (Show)

pOptions :: Parser StoreOption
pOptions = StoreOption <$> strOption (long "config" <> short 'c' <> help "store option config file")



main :: IO ()
main = do
  opt <- execParser (info pOptions (fullDesc <> progDesc "store management CLI tool"))
  ecfg :: Either String StorageConfig <-
    eitherDecodeStrict <$> B.readFile (storeConfig opt)
  print ecfg

