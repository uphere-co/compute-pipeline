{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Lens           ((^..),at,ix,only)
import           Control.Monad          (forM_)
import           Data.Foldable          (toList)
import           Data.List              (sort)
import           Data.Text              (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO   as TIO
import           System.Directory.Tree  (dirTree, readDirectoryWith)
import           Text.Taggy.Lens

data ArticleNYT = ArticleNYT
  { _title :: [Maybe Text]
  , _link :: [Maybe Text]
  , _ptime :: [Maybe Text]
  , _summary :: [Maybe Text]
  , _maintext :: [Text]
  } deriving (Show)

parseNYT :: Text -> ArticleNYT
parseNYT htxt =
  let ltxt = TL.fromStrict htxt
      ml = ltxt ^.. html . allNamed (only "meta")
      ll = ltxt ^.. html . allNamed (only "link")
      pl = ltxt ^.. html . allNamed (only "p")
             
      title   = ml ^.. traverse . attributed (ix "property" . only "og:title") . attrs . at "content"
      summary = ml ^.. traverse . attributed (ix "property" . only "og:description") . attrs . at "content"
      ptime   = ml ^.. traverse . attributed (ix "name" . only "ptime") . attrs . at "content"
      link    = ll ^.. traverse . attributed (ix "rel" . only "canonical") . attrs . at "href"
      maintext1 = pl ^.. traverse . attributed (ix "class" . only "story-body-text story-content") . children . traverse . content
      maintext2 = pl ^.. traverse . attributed (ix "itemprop" . only "articleBody") . children . traverse . content

  in ArticleNYT { _title = title
                , _link  = link
                , _ptime = ptime
                , _summary = summary
                , _maintext = (maintext1 ++ maintext2)
                }


main :: IO ()
main = do
  filelist' <- readDirectoryWith return "/data/modori/NYT.dump"
  let filelist = sort . toList $ dirTree filelist'

  forM_ (take 1 filelist) $ \f -> do
    txt <- TIO.readFile f
    let parsedNYT = parseNYT txt
    print parsedNYT
