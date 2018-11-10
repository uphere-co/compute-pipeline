{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Concurrent.STM   ( TVar, newTVarIO, readTVarIO, writeTVar
                                          , atomically  )
import           Control.Concurrent.STM.TChan ( TChan, newBroadcastTChanIO
                                              , dupTChan, readTChan, writeTChan )
import           Control.Monad            ( forever, when )
import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(ExceptT) )
import           Data.Aeson               ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Data.List                ( find )
import           Data.Text                ( Text )
import qualified Data.Text as T
import           Network.Wai.Handler.Warp ( runSettings, defaultSettings, setBeforeMainLoop, setPort )
import           Network.WebSockets       ( Connection, forkPingThread, sendBinaryData )
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
import           Compute.Type             ( OrcApi, SOInfo(..), orcApi )


-- * app

runApp :: (ComputeConfig,FilePath) -> IO ()
runApp (cfg,sofile) = do
  let port = 3123
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
      soinfo = SOInfo sofile
  ref <- newTVarIO soinfo
  chan <- newBroadcastTChanIO
  runSettings settings $ serve orcApi (server (cfg,soinfo) ref chan)


server :: (ComputeConfig,SOInfo)
       -> TVar SOInfo
       -> TChan SOInfo  -- ^ write-only broadcast channel
       -> Server OrcApi
server (cfg,soinfo) ref chan =
       wsStream chan
  :<|> getCompute cfg
  :<|> getCell cfg
  :<|> getSO soinfo
  :<|> postUpdate ref chan


getCompute :: ComputeConfig -> Handler ComputeConfig
getCompute cfg = pure cfg

getCell :: ComputeConfig -> Text -> Handler CellConfig
getCell cfg name =
  let mc = find (\c -> cellName c == name) (computeCells cfg)
  in case mc of
       Nothing -> throwError err404
       Just c  -> pure c

getSO :: SOInfo -> Handler Text
getSO (SOInfo fp) = pure (T.pack fp)

postUpdate :: TVar SOInfo -> TChan SOInfo -> Text -> Handler ()
postUpdate ref chan txt = liftIO $ do
  let fp = T.unpack txt
  SOInfo fp' <- readTVarIO ref
  when (fp /= fp') $ do
    print fp
    atomically $ do
      writeTVar ref (SOInfo fp)
      writeTChan chan (SOInfo fp)


wsStream :: TChan SOInfo -> Connection -> Handler ()
wsStream chan c = liftIO $ do
  chan' <- atomically (dupTChan chan)
  forkPingThread c 10
  forever $ do
    soinfo <- atomically $ readTChan chan'
    sendBinaryData c soinfo -- (T.pack fp)
  

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
