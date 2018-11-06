{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(ExceptT) )
import           Data.Aeson               ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Data.List                ( find )
import           Data.Text                ( Text )
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setBeforeMainLoop, setPort )
import           Options.Applicative      ( Parser, (<**>)
                                          , execParser, help, helper
                                          , long, info, progDesc, short
                                          , strOption
                                          )
import           Servant                  ( Handler, Server, (:<|>)((:<|>))
                                          , err404, throwError
                                          , serve
                                          )
import           System.IO                ( hPutStrLn, stderr )
------
import           CloudHaskell.Type        ( handleError )
import           Worker.Type              ( ComputeConfig(..), CellConfig(..) )
------
import           Compute.Type             ( OrcApi, orcApi )


-- * app

runApp :: ComputeConfig -> IO ()
runApp cfg = do
  let port = 3123
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ serve orcApi (server cfg)

server :: ComputeConfig -> Server OrcApi
server cfg = getCompute cfg :<|> getCell cfg

--  getItems :<|> getItemById

getCompute :: ComputeConfig -> Handler ComputeConfig
getCompute cfg = pure cfg

getCell :: ComputeConfig -> Text -> Handler CellConfig
getCell cfg name =
  let mc = find (\c -> cellName c == name) (computeCells cfg)
  in case mc of
       Nothing -> throwError err404
       Just c  -> pure c


data OrcOpt = OrcOpt { computeConfigFile :: FilePath }
            deriving Show

pOptions :: Parser OrcOpt
pOptions = OrcOpt <$> strOption ( long "compute"
                               <> short 'c'
                               <> help "Compute pipeline configuration"
                                )


main :: IO ()
main = do
  handleError $ do
    opt <- liftIO $ execParser (info (pOptions <**> helper) (progDesc "orchestrator"))
    compcfg :: ComputeConfig <-
      ExceptT $
        eitherDecodeStrict <$> B.readFile (computeConfigFile opt)
    liftIO $ runApp compcfg
