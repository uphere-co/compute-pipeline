{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Database.Beam
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PGS

import DB.Schema.RSS
import DB.Schema.RSS.Article
import DB.Schema.RSS.CoreNLP
import DB.Schema.RSS.Summary

main = do
  conn <- PGS.connectPostgreSQL ""
  lst :: [(RSSArticle,Summary)] -- [AnalysisCoreNLP]
    <- runBeamPostgresDebug putStrLn conn $
         runSelectReturningList $
           select $ do
             let hsh = ""
             --  c <- all_ (_coreNLPs rssDB)
             a <- all_ (_rssArticles rssDB)
             let hsh = a^.rssArticleHash
             s <- filter_ (\s -> s^.summaryHash ==. hsh) (all_ (_summaries rssDB))
             guard_ . not_ . exists_ . filter_ (\c -> c^.coreNLPHash ==. hsh) $ all_ (_coreNLPs rssDB)
             pure (a,s)

  print (length lst)
