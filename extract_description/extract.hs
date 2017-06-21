{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Function                (on)
import           Data.List                    (sort,sortBy)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid                  ((<>))
import qualified Data.Text.IO          as TIO
import           System.Directory
import           System.FilePath
--
import           NewsAPI.Type (SourceArticles(..))

printFormat (time,title,desc) = do
  TIO.putStrLn time
  TIO.putStrLn title
  TIO.putStrLn desc
  TIO.putStrLn ""
                

main :: IO ()
main = do
  let dir = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg"
  cnts <- getDirectoryContents dir
  let cnts' = (filter (\x -> x /= "." && x /= "..")) cnts
  lst <- flip mapM cnts' $ \fp -> do
    bstr <- B.readFile (dir </> fp)
    let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
    case esrc of
      Left err -> return Nothing
      Right src -> return ((,,) <$> _publishedAt src <*> _title src <*> _description src)

  mapM_ printFormat $ sortBy (compare `on` (^._1)) $ catMaybes lst 
