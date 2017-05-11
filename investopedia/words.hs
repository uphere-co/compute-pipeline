{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Lens hiding (children,element)
import           Data.Maybe (mapMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import Data.Text.Lazy as TL
import System.Environment
import System.IO
import System.Process
import Text.Taggy.Lens

extract c = c^.attr "data-label"

lst' = [("a",13)
      ,("b",11)
      ,("c",18)
      ,("d",9)
      ,("e",7)
      ,("f",9)
      ,("g",5)]

lst = [("h",4)
      ,("i",8)
      ,("j",2)
      ,("k",1)
      ,("l",6)
      ,("m",9)
      ,("n",6)
      ,("o",4)
      ,("p",10)
      ,("q",2)
      ,("r",8)
      ,("s",16)
      ,("t",8)
      ,("u",4)
      ,("x",1)
      ,("y",1)
      ,("z",1)
      ,("1",1)]


main = flip mapM_ lst $ \(l,n) -> do
         flip mapM_ ([1..n] :: [Int]) $ \i -> process l i >> threadDelay 1000000
  -- args <- getArgs
  -- let n :: Int = read (args !! 1) 


process letter n = do
  let url = "http://www.investopedia.com/terms/" ++ letter ++
              if n == 1 then "/" else "/?page=" ++ show n
  print url 
  str <- readProcess "curl" [url] []
  let txt = TL.pack str
      body = Prelude.head (txt ^.. html . allNamed (only "body"))
      contents = body ^.. allAttributed (ix "data-cat" . only "content_list")

  TIO.writeFile (letter ++ show n ++ ".txt") $ T.unlines $ mapMaybe extract contents
