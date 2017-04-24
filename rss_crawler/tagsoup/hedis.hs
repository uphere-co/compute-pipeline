{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Crypto.Hash.SHA256
import qualified Data.ByteString.Char8  as B
import           Data.ByteString.Base16
import           Data.Function               (on)
import           Data.List              as L
import Database.Redis
import Text.Printf

format (x,y) = B.unpack x ++ "  " ++ B.unpack y

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo { connectHost = "node1", connectPort = PortNumber 8937 }
  runRedis conn $ do
    elinks <- smembers "processed_nyt_links"
    case elinks of
      Left rply -> liftIO $ print rply
      Right links -> do
        let pairs = L.sortBy (compare `on` fst) $ map (\x -> (encode (hash x), x)) links
        liftIO $ mapM_ (putStrLn . format) pairs
        


                                               
