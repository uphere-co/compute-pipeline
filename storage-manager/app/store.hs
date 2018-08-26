{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT(..),runExceptT)
import           Data.Aeson                 (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup             ((<>))
import           Options.Applicative        (Parser,(<**>)
                                            ,command,execParser,help,helper,info,long
                                            ,progDesc
                                            ,short,strOption,subparser)
--
import           Storage.Config             (StorageConfig)


data ProgOption = ProgOption {
                     storeConfig :: FilePath
                     }
                 deriving (Show)


data ProgCommand = Register ProgOption
                 | Install  ProgOption


pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "config" <> short 'c' <> help "store option config file")


pCommand :: Parser ProgCommand
pCommand =
  subparser
    ( command "register" (info (Register <$> pOptions) (progDesc "register new package"))
   <> command "install"  (info (Install  <$> pOptions) (progDesc "install package into current directory")))


doRegister :: StorageConfig -> ExceptT String IO ()
doRegister cfg = do
  liftIO $ putStrLn "register"
  liftIO $ print cfg

doInstall :: StorageConfig -> ExceptT String IO ()
doInstall cfg = do
  liftIO $ putStrLn "install"
  liftIO $ print cfg


parseConfig :: ProgOption -> ExceptT String IO StorageConfig
parseConfig opt = ExceptT (eitherDecodeStrict <$> B.readFile (storeConfig opt))


main :: IO ()
main = do
  cmd <- execParser (info (pCommand <**> helper) (progDesc "store management CLI tool"))
  r <- case cmd of
         Register opt -> runExceptT $ do
           cfg <- parseConfig opt
           doRegister cfg
         Install  opt -> runExceptT $ do
           cfg <- parseConfig opt
           doInstall cfg
  print r
