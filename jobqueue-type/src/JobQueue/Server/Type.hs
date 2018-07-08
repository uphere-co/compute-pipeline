module JobQueue.Server.Type where

import HEP.Storage.WebDAV.Type

data ServerConfig = ServerConfig { 
  server_main   :: String, 
  server_webdav :: URLtype -- WebDAVServer 
} deriving (Show)
