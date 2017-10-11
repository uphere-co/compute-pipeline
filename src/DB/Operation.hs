{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.Operation where

import           Control.Arrow
import           Control.Monad.IO.Class                     (liftIO)
import           Data.ByteString.Char8                      (ByteString)
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Maybe                                 (fromJust,isNothing)
import           Data.Text                                  (Text)
import qualified Data.Text                      as T
import           Data.Time.Clock                            (UTCTime,getCurrentTime)
import qualified Database.PostgreSQL.Simple     as PGS
import           Opaleye                        hiding      (constant)
import           Opaleye.Internal.Column                    (unColumn)
--                                                                                                                            
import           Model.Opaleye.ShowConstant                 (constant,safeCoerceToRep)
import           Model.Opaleye.To
import           DB.Schema.NewsAPI.Article                  (Article, ArticleP(Article), ArticleH)
import qualified DB.Schema.NewsAPI.Article      as A
--
import           DB.Type

uploadArticle :: (ToRSSArticle a) => PGS.Connection -> a -> IO ()
uploadArticle conn x = do
  let a = toRSSArticle x
  runInsert conn A.table $
    A.newArticle (_rss_hash a) (_rss_source a) (_rss_created a)
  return ()
