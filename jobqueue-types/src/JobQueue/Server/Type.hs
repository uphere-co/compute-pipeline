module JobQueue.Server.Type where

import Storage.Type

data ServerConfig = ServerConfig { 
  server_main   :: String, 
  server_webdav :: Path
} deriving (Show)
