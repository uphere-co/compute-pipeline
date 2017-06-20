{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.List                    (sort)
import           Data.Monoid                  ((<>))
import qualified Data.Text.IO          as TIO
import           System.Directory
import           System.FilePath
--
import           NewsAPI.Type (SourceArticles(..))


main :: IO ()
main = do
  let dir = "/data/groups/uphere/intrinio/Articles/bloomberg"
  cnts <- getDirectoryContents dir
  let cnts' = (sort . filter (\x -> x /= "." && x /= "..")) cnts
  flip mapM_  cnts' $ \fp -> do
    bstr <- B.readFile (dir </> fp)
    let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
    case esrc of
      Left err -> return ()
      Right src -> do
        mapM_ (TIO.putStrLn . (<> ".")) (_title src)
        mapM_ TIO.putStrLn (_description src)
        TIO.putStrLn ""
        
        -- print (._title src,_description src)
    
  {- 
  txt <- TIO.readFile ""
  -}
