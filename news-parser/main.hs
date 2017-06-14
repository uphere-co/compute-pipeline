{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens          ((^..),at,ix,only)
import           Control.Monad         (forM_)
import           Data.Foldable         (toList)
import           Data.List             (sort)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO   as TIO
import           System.Directory.Tree (dirTree, readDirectoryWith)
import           Text.Taggy.Lens


main :: IO ()
main = do
  filelist' <- readDirectoryWith return "/data/modori/NYT.dump"
  let filelist = sort . toList $ dirTree filelist'

  forM_ (take 1 filelist) $ \f -> do
    txt <- TIO.readFile f
    let ml = (TL.fromStrict txt) ^.. html . allNamed (only "meta")
        pl = (TL.fromStrict txt) ^.. html . allNamed (only "p")
    print $ ml ^.. traverse . attributed (ix "property" . only "og:title") . attrs . at "content"
    print $ ml ^.. traverse . attributed (ix "property" . only "og:description") . attrs . at "content"
    print $ pl ^.. traverse . attributed (ix "class" . only "story-body-text story-content") . children . traverse . content
    print $ pl ^.. traverse . attributed (ix "itemprop" . only "articleBody") . children . traverse . content
