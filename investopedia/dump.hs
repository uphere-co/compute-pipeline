{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Lens hiding (children,element)
import qualified Data.List.Split  as DLS
import           Data.Maybe (mapMaybe)
import qualified Data.Text.IO as TIO
import Data.Text              (Text)
import qualified Data.Text    as T
import Data.Text.Lazy as TL
import System.Directory       (doesFileExist, listDirectory)
import System.Environment
import System.IO
import System.Process
import Text.Taggy.Lens
import Network.HTTP.Conduit
import Network.HTTP.Simple


main :: IO ()
main = do
  dir <- listDirectory "data"
  print dir
  txt <- mapM TIO.readFile (Prelude.map ("data/" ++) dir)
  print txt
  
  let lines = Prelude.concat $ Prelude.map (\x-> Prelude.map Prelude.last $ Prelude.map (T.split (=='|')) $ Prelude.filter (\x -> T.length x > 0) $ T.lines x) txt
      url = Prelude.map (T.append "http://www.investopedia.com") (Prelude.map (T.drop 1) lines)
  flip mapM_ (Prelude.map T.unpack url) $ \u -> do
      {-
      str <- readProcess "wget" [u] []
      let txt = TL.pack str
          div = txt ^.. html . allNamed (only "div") . allAttributed (folded. only "content-box content-box-term")
      --          contents = body ^.. allAttributed (ix "data-cat" . only "content_list")
      -- print (i,div)
      -}
      let file = Prelude.last $ DLS.splitOn "/" u
      print file
      filechk <- doesFileExist file
      if filechk then (return ()) else ((callCommand $ "wget " ++ u) >> threadDelay 500000)

      -- body = Prelude.head (txt ^.. html)
      -- TIO.writeFile "txt" $ T.unlines $ mapMaybe extract contents
                                          
  -- mapM_ print url
  return ()
  
