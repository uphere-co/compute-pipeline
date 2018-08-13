{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pipeline.App.CoreNLP where

import           Control.Concurrent                (threadDelay)
import           Control.Exception                 (SomeException,try)

import           Control.Lens                      ((&),(^.),(.~),set)
import           Control.Monad                     (forever,forM_)
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import           Data.Default                      (def)
import           Data.List                         (foldl')
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time.Clock                   (getCurrentTime)
import           Database.Beam                     (select,runSelectReturningList
                                                   ,update,runUpdate
                                                   ,insert,runInsert,insertValues
                                                   ,guard_,val_,all_
                                                   ,(==.),(<-.))
import           Database.Beam.Postgres            (runBeamPostgres)

import           Language.Java         as J
import           System.Environment                (getEnv)
--
import           CoreNLP.Simple                    (prepare)
import           CoreNLP.Simple.Type               (constituency,lemma,ner,postagger
                                                   ,sutime,tokenizer,words2sentences)
import           DB.Operation.RSS.ErrorArticle
import           DB.Schema.RSS
import           DB.Schema.RSS.Article
import           DB.Schema.RSS.CoreNLP
import           DB.Schema.RSS.ErrorArticle
import           NLP.Shared.Type                   (Summary,PathConfig,dbstring,description)
import           SRL.Analyze.CoreNLP               (runParser)
--
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Source.RSS.Article       (listNewArticles)
import           Pipeline.Type                     (SourceTimeConstraint)





--
--
-- import           Pipeline.Operation.DB


-- this is only for market pulse source
noisyTextClips :: [Text]
noisyTextClips =
  [ "Market Pulse Stories are Rapid-fire, short news bursts on stocks and markets as they move. Visit MarketWatch.com for more information on this news."
  ]

-- this is only for Reuters
tameDescription :: Text -> Text
tameDescription txt = snd $ T.breakOnEnd "(Reuters) - " $ txt


cleanNoiseText :: Text -> Text
cleanNoiseText txt0 = let txt1 = tameDescription txt0
                          txt2 = foldl' (\(!txt) clip -> T.replace clip "" txt) txt1 noisyTextClips
                      in T.strip txt2



preprocessRSSArticle :: (RSSArticle,Summary) -> Maybe (RSSArticle,Summary)
preprocessRSSArticle (article,item) =
  let txt1 = cleanNoiseText (item ^. description)
  in if | T.null txt1        -> Nothing
        | T.head txt1 == '*' -> Nothing
        | otherwise          ->
            let -- this is real extreme ad hoc
                ws = T.words txt1
            in if (not . null . filter (\w -> w `elem` ["*","**"])) ws
               then Nothing
               else let txt = T.intercalate " " (T.words txt1)
                    in Just (article,(set description txt item))




preParseRSSArticles :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                    -> PathConfig
                    -> [(RSSArticle,Summary)]
                    -> IO ()
preParseRSSArticles pp cfg articles = do
  conn <- getConnection (cfg ^. dbstring)
  let preprocessed = mapMaybe preprocessRSSArticle articles
  forM_ preprocessed $ \(article,item) -> do
    let hsh = article^.rssArticleHash
        src = article^.rssArticleSource
        txt = item^.description
    eresult <- try $ runParser pp txt
    case eresult of
      Left  (e :: SomeException) -> do
        let errmsg = T.pack (show e)
        let err = RSSErrorArticle hsh src errmsg (article^.rssArticleCreated)
        uploadRSSErrorArticleIfMissing conn err
      Right result                -> do
        time <- getCurrentTime
        let rtxt = TE.decodeUtf8 (BL.toStrict (A.encode result))
        -- TODO: This is to insert if not exist and update if exist.
        --       We had better refactor this out as a utility.
        --       Pipeline.Run.Analysis has duplicated code.
        as'  <- runBeamPostgres conn $
                  runSelectReturningList $
                    select $ do
                      c <- all_ (_coreNLPs rssDB)
                      guard_ (c^.coreNLPHash ==. val_ hsh)
                      pure c
        case as' of
          []  -> let corenlp :: AnalysisCoreNLP
                     corenlp = AnalysisCoreNLP hsh (Just rtxt) time
                 in runBeamPostgres conn $
                      runInsert $
                        insert (_coreNLPs rssDB) $
                          insertValues [corenlp]
          _as -> runBeamPostgres conn $
                   runUpdate $
                     update (_coreNLPs rssDB)
                            (\corenlp -> [ corenlp^.coreNLPResult  <-. val_ (Just rtxt)
                                         , corenlp^.coreNLPCreated <-. val_ time
                                         ])
                            (\corenlp -> corenlp^.coreNLPHash ==. val_ hsh)
        -- updateRSSAnalysisStatus conn hsh (Just True,Nothing,Nothing)
  closeConnection conn



runCoreNLPforRSS :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                 -> PathConfig
                 -> SourceTimeConstraint
                 -> IO ()
runCoreNLPforRSS pp cfg sc = do
  articles <- listNewArticles cfg sc
  preParseRSSArticles pp cfg articles


-- ------------------------------------------------------------------------

runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  clspath <- getEnv "CLASSPATH"
  conn <- getConnection (cfg ^. dbstring)
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    forever $ do
      -- TODO: reuters/Archive should be separated out as a configuration
      runCoreNLPforRSS pp cfg (Just "reuters/Archive",Nothing)
      putStrLn "Waiting next run..."
      -- TODO: this time should be configured.
      let sec = 1000000 in threadDelay (600*sec)

  closeConnection conn
