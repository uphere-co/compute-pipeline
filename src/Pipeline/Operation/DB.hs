{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Operation.DB where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Database.PostgreSQL.Simple as PGS

getConnection config = PGS.connectPostgreSQL ((BL8.toStrict . BL8.pack) config)
