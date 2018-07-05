{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad                     (filterM,forever,forM_)
import qualified Data.ByteString.Char8 as B
import           Data.IntMap                       (IntMap)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import           Data.Tree                         (Forest)
import qualified Database.PostgreSQL.Simple as PGS
import           SRL.Analyze                       (loadConfig)
import           SRL.Analyze.Type                  (AnalyzePredata(..))
import           System.Directory                  (doesFileExist)
import           System.Environment                (getEnv)
import           System.FilePath                   ((</>),addExtension)
--
import           Lexicon.Data                      (loadLexDataConfig)
import           NER.Type                          (CompanyInfo,alias)
import           NLP.Shared.Type                   (PathConfig,corenlpstore,dbstring,lexconfigpath,mgstore)
import           NLP.Type.CoreNLP
import           Text.Search.Generic.SearchTree    (addTreeItem)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Operation.DB
import           Pipeline.Run.Analysis
import           Pipeline.Run.CoreNLP
import           Pipeline.Type


type Source = String
type Section = String
type RSSLink = String


constraint :: SourceTimeConstraint
constraint = (Just "reuters/Archive",Nothing)



runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger,forest,companyMap) <- loadConfig (False,False) cfgG
  forever $ do
    runSRL conn apredata netagger (forest,companyMap) cfg constraint
    putStrLn "Waiting next run..."
    let sec = 1000000 in threadDelay (60*sec)
  closeConnection conn



