{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Lens        hiding (children,element)
import           Control.Monad
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Text.Lazy     as TL
import qualified Data.Text.Lazy.IO  as TLIO
import           System.Environment
import           System.IO
import           System.Process
import           Text.Taggy.Lens

hasPrefix :: T.Text -> Prism' T.Text ()
hasPrefix p = nearly "" (p `T.isPrefixOf`)

                         
getContent :: Node -> T.Text
getContent (NodeContent txt) = txt --  Prelude.concatMap (x ^.. children.traverse.content)
getContent (NodeElement e) = T.concat (e ^.. children.traverse.content)

main = do
  -- txt <- TLIO.readFile "goingpublic.asp"
  txt <- TLIO.readFile "spinoff.asp"  
  let body = Prelude.head (txt ^.. html . allNamed (only "body"))
      layoutbody = body ^.. allAttributed (ix "class" . hasPrefix "layout-body ")
      xs = do
        l <- layoutbody
        y <- l ^.. allNamed ((only "h2") `failing` (only "p"))
        z <- join (y ^.. children)
        return (getContent z)
  mapM_ TIO.putStrLn xs
