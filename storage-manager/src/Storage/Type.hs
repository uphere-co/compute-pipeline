{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Storage.Type where

import Data.Typeable
import Data.Data

-- | 
data URLtype = LocalURL FilePath
             | GlobalURL String
             deriving (Show)

-- | 
data WebDAVProgram = UseCurl (Maybe FilePath) 
--  | Cadaver (Maybe FilePath)

-- | 
data Credential = CredDigest String String | NoCred

-- | 
data WebDAVConfig = WebDAVConfig { webdav_credential :: Credential 
                                 , webdav_baseurl   :: String 
                                 } 

data WebDAVRemoteDir = WebDAVRemoteDir {  webdav_remotedir :: FilePath } 
                       deriving (Show, Typeable, Data)


data WebDAVCommand = Download | Upload
  deriving (Show, Typeable, Data)
