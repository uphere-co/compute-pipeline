{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Lens        hiding (children,element,(<.>))
import           Control.Monad
import           Data.List                  (sort)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TLIO
import           System.Directory           (getDirectoryContents)
import           System.Environment         (getArgs)
import           System.FilePath            ((</>),(<.>),takeBaseName,takeExtensions)
import           System.IO
import           System.Process
import           Text.Taggy.Lens

hasPrefix :: T.Text -> Prism' T.Text ()
hasPrefix p = nearly "" (p `T.isPrefixOf`)

                         
getContent :: Node -> T.Text
getContent (NodeContent txt) = txt --  Prelude.concatMap (x ^.. children.traverse.content)
getContent (NodeElement e) = T.concat (e ^.. children.traverse.content)

main = do
  dir <- (!! 0) <$> getArgs
  dirtarget <- (!! 1) <$> getArgs
  -- txt <- TLIO.readFile "goingpublic.asp"
  -- txt <- TLIO.readFile "spinoff.asp"
  contents0 <- getDirectoryContents dir
  let contents = sort $ filter (\f -> takeExtensions f == ".asp") contents0
  forM_ contents $ \f -> do
    let fullpath = dir </> f
        target = dirtarget </> takeBaseName f <.> "txt"
    process fullpath target
    -- mapM_ print contents

process fp tfp = do
  withFile tfp WriteMode $ \h -> do
    putStrLn fp
    txt <- TLIO.readFile fp
    let body = Prelude.head (txt ^.. html . allNamed (only "body"))
        layoutbody = body ^.. allAttributed (ix "class" . hasPrefix "layout-body ")
        xs = do
          l <- layoutbody
          y <- l ^.. allNamed ((only "h2") `failing` (only "p"))
          z <- join (y ^.. children)
          return (getContent z)
    let result = T.intercalate "\n" xs
    TIO.hPutStrLn h result
    
  -- mapM_ TIO.putStrLn xs
