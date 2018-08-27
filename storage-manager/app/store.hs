{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT(..),runExceptT,throwE)
import           Data.Aeson                 (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup             ((<>))
import           Data.UUID                  (UUID,fromString)
import           Options.Applicative        (Parser,(<**>)
                                            ,command,execParser,help,helper,info,long
                                            ,progDesc
                                            ,short,strOption,subparser)
--
import           Storage.Config             (StorageConfig)
import           Storage.Operation          (register,install)


data ProgOption = ProgOption {
                     storeConfig :: FilePath
                     }
                 deriving (Show)


data ProgCommand = Register ProgOption FilePath
                 | Install  ProgOption (Maybe UUID)


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "config" <> short 'c' <> help "store option config file")

pFilePath :: Parser FilePath
pFilePath = strOption (long "directory" <> short 'd' <> help "filepath to make a target package")

pUUID :: Parser (Maybe UUID)
pUUID = fromString <$> strOption (long "uuid" <> short 'u' <> help "UUID for a package")


pCommand :: Parser ProgCommand
pCommand =
  subparser
    ( command "register" (info (Register <$> pOptions <*> pFilePath)
                               (progDesc "register new package"))
   <> command "install"  (info (Install  <$> pOptions <*> pUUID)
                               (progDesc "install package into current directory")))




parseConfig :: ProgOption -> ExceptT String IO StorageConfig
parseConfig opt = ExceptT (eitherDecodeStrict <$> B.readFile (storeConfig opt))


main :: IO ()
main = do
  cmd <- execParser (info (pCommand <**> helper) (progDesc "store management CLI tool"))
  r <- case cmd of
         Register opt fp -> runExceptT $ do
           cfg <- parseConfig opt
           register cfg fp
         Install  opt muuid -> runExceptT $ do
           case muuid of
             Nothing   -> throwE "UUID is not valid"
             Just uuid -> do
               cfg <- parseConfig opt
               install cfg uuid
  print r
