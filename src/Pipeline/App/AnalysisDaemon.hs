{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad                     (filterM,forever,forM_)
import qualified Data.ByteString.Char8 as B
import           Data.IntMap                       (IntMap)
import           Data.List.Split                   (chunksOf)
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
-- import           NewsAPI.Type
import           NER.Load                          (loadCompanies)
import           NER.Type                          (CompanyInfo,alias)
import           NLP.Shared.Type                   (PathConfig,corenlpstore,dbstring,lexconfigpath,mgstore)
import           NLP.Type.CoreNLP
-- import           RSS.Data                          (rssAnalysisList)
import           Text.Search.Generic.SearchTree    (addTreeItem)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.Concurrent
import           Pipeline.Operation.DB
import           Pipeline.Run.CoreNLP
import           Pipeline.Source.RSS.Article       (listNewDocAnalysisInputs)
-- import           Pipeline.Source.RSS.Analysis
import           Pipeline.Type


type Source = String
type Section = String
type RSSLink = String


constraint :: SourceTimeConstraint
constraint = (Just "reuters/Archive",Nothing)

{-
    {- ("reuters","companyNews","http://feeds.reuters.com/reuters/companyNews")
  , ("reuters","technologyNews","http://feeds.reuters.com/reuters/technologyNews")
  , -} ("reuters","Archive","http://www.reuters.com/resources/archive/us")
  {- , ("cnbc","business","https://www.cnbc.com/id/10001147/device/rss/rss.html")
  , ("cnbc","economy","https://www.cnbc.com/id/20910258/device/rss/rss.html")
  , ("cnbc","finance","https://www.cnbc.com/id/10000664/device/rss/rss.html")
  , ("cnbc","technology","https://www.cnbc.com/id/19854910/device/rss/rss.html")
  , ("marketwatch","topstories","http://feeds.marketwatch.com/marketwatch/topstories")
  , ("marketwatch","marketpulse","http://feeds.marketwatch.com/marketwatch/marketpulse") -- Paragraph  -}
  ]
-}


    -- forM_ prestigiousNewsSource $ \src -> runSRL conn apredata netagger cfg src

runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger,forest,companyMap) <- loadConfig (False,False) cfgG
  forever $ do
    {- forM_ rssAnalysisList $ \(src,sec,url) -> do
      print (src,sec,url)
      runSRL conn apredata netagger (forest,companyMap) cfg (T.pack (src ++ "/" ++ sec)) -}
    runSRL conn apredata netagger (forest,companyMap) cfg constraint
    putStrLn "Waiting next run..."

    let sec = 1000000 in threadDelay (60*sec)
  closeConnection conn

coreN = 15 :: Int

-- | This does SRL and generates meaning graphs.
--
runSRL :: PGS.Connection
       -> AnalyzePredata
       -> ([Sentence] -> [EntityMention T.Text])
       -> (Forest (Either Int Text), IntMap CompanyInfo)
       -> PathConfig
       -> SourceTimeConstraint
       -> IO ()
runSRL conn apredata netagger (forest,companyMap) cfg (msrc,tc) = do
  loaded <- listNewDocAnalysisInputs cfg (msrc,tc)
  let n = length loaded `div` coreN
  forM_ (chunksOf n loaded) $ \ls ->
    forkChild (runAnalysisByChunks conn netagger (forest,companyMap) apredata cfg ls)

  waitForChildren
  refreshChildren


    -- print ls


  -- print as1b
  -- let as1 = (take 5000 as1b) -- as1a ++ as1b
  -- print as1

  -- print (length as1)
  -- mapM_ print as1

  -- loaded1 <- loadCoreNLPResult $ map (\(fp,tm) -> ((cfg ^. corenlpstore) </> fp, tm)) as1
  -- let loaded = catMaybes $ map (\(a,b,c) -> (,,) <$> Just a <*> Just b <*> c) (catMaybes loaded1)
