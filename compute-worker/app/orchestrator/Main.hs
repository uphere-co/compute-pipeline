{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Concurrent.MVar  ( MVar, modifyMVar, newMVar )
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Text                ( Text )
import           GHC.Generics
import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application, responseBuilder )
import           Network.Wai.Handler.Warp ( run, runSettings, defaultSettings, setBeforeMainLoop, setPort )
import           Servant
import           System.IO

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy


-- * app

runApp :: IO ()
runApp = do
  let port = 3123
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings $ serve itemApi server

server :: Server ItemApi
server = getItems :<|> getItemById

getItems :: Handler [Item]
getItems = pure [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \case
  0 -> pure exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- *

data Item = Item { itemId :: Integer
                 , itemText :: Text
                 }
          deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item


application :: MVar Int -> Application
application countRef _ respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 1102
        msg =    fromByteString (B.pack (show count'))
    responseReceived <-
      respond $
        responseBuilder
          status200
          [("Content-Type", "text/plain")]
          msg
    pure (count',responseReceived)

main :: IO ()
main = do
  putStrLn "orchestrator starts"
  {- ref <- newMVar (0 :: Int)
  run 32929 $ application ref
  -}
  runApp
