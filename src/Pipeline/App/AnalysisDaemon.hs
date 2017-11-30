{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisDaemon where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad                     (filterM,forever,forM_)
import qualified Data.ByteString.Char8 as B
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
import           NER.Type                          (alias)
import           NLP.Shared.Type                   (PathConfig,corenlpstore,dbstring,lexconfigpath,mgstore)
import           NLP.Type.CoreNLP
import           RSS.Data                          (rssAnalysisList)
import           Text.Search.Generic.SearchTree    (addTreeItem)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.App.AnalysisRunner
import           Pipeline.Load
import           Pipeline.Operation.Concurrent
import           Pipeline.Operation.DB
import           Pipeline.Run.CoreNLP
import           Pipeline.Source.RSS.Analysis
import           Pipeline.Type

runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger) <- loadConfig False cfgG
  companies <- loadCompanies
  let clist = concat $ map (^. alias) companies 
      forest = foldr addTreeItem [] (map T.words clist)
  forever $ do
    -- forM_ prestigiousNewsSource $ \src -> runSRL conn apredata netagger cfg src
    forM_ rssAnalysisList $ \(src,sec,url) -> runSRL conn apredata netagger forest cfg (src ++ "/" ++ sec)
    putStrLn "Waiting next run..."
    let sec = 1000000 in threadDelay (60*sec)
  closeConnection conn

coreN = 15 :: Int

-- | This does SRL and generates meaning graphs.
--
runSRL :: PGS.Connection -> AnalyzePredata -> ([Sentence] -> [EntityMention T.Text]) -> Forest (Maybe Text) -> PathConfig -> String  -> IO ()
runSRL conn apredata netagger forest cfg src = do
  as1b <- getNewItemsForSRL cfg src
  let as1 = (take 5000 as1b) -- as1a ++ as1b
 
  loaded1 <- loadCoreNLPResult $ map (\(fp,tm) -> ((cfg ^. corenlpstore) </> fp, tm)) as1
  let loaded = catMaybes $ map (\(a,b,c) -> (,,) <$> Just a <*> Just b <*> c) (catMaybes loaded1)
  let (n :: Int) = let n' = ((length loaded) `div` coreN) in if n' >= 1 then n' else 1
  forM_ (chunksOf n loaded) $ \ls -> do
    forkChild (runAnalysisByChunks conn netagger forest apredata cfg ls)

  waitForChildren
  refreshChildren
