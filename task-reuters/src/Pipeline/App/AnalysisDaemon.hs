{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad                     (forever)
import           SRL.Analyze                       (loadConfig)
--
import           Lexicon.Data                      (loadLexDataConfig)
import           NLP.Shared.Type                   (PathConfig,dbstring,lexconfigpath)
--
import           Pipeline.Operation.DB
import           Pipeline.Run.Analysis
import           Pipeline.Type


type Source = String
type Section = String
type RSSLink = String


constraint :: SourceTimeConstraint
constraint = (Just "reuters/Archive",Nothing)



runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right c -> return c;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger,forest,companyMap) <- loadConfig (False,False) cfgG
  forever $ do
    runSRL conn apredata netagger (forest,companyMap) cfg constraint
    putStrLn "Waiting next run..."
    let sec = 1000000 in threadDelay (60*sec)
  closeConnection conn



