{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Pipeline.Run.CoreNLP where

import           Control.Exception                            (SomeException,try)
import           Control.Lens                                 ((^.),_1,_2,set,to)
import           Control.Monad                                (forM_,join,when)
import qualified Data.Aeson                            as A
import qualified Data.ByteString.Lazy.Char8            as BL
import           Data.List                                    (foldl')
import           Data.Maybe
import           Data.Text                                    (Text)
import qualified Data.Text                             as T
import           Language.Java                         as J
import           System.FilePath                              ((</>))
--
import           NLP.Shared.Type                              (Summary,PathConfig,corenlpstore,dbstring,errstore,description)

import           DB.Operation.RSS.Analysis                    (updateRSSAnalysisStatus)
import           DB.Operation.RSS.Article
import           DB.Operation.RSS.ErrorArticle
import           DB.Schema.RSS.Article
import           DB.Schema.RSS.ErrorArticle
import           DB.Util                                      (b16ToBstrHash,bstrHashToB16)
import           SRL.Analyze.CoreNLP                          (runParser)
--
import qualified Pipeline.Source.RSS.Article           as RSS
import           Pipeline.Operation.DB
import           Pipeline.Type
import           Pipeline.Util


noisyTextClips =
  [ "Market Pulse Stories are Rapid-fire, short news bursts on stocks and markets as they move. Visit MarketWatch.com for more information on this news." 
  ]

tameDescription :: Text -> Text
tameDescription txt = snd $ T.breakOnEnd "(Reuters) - " $ txt

cleanNoiseText :: Text -> Text
cleanNoiseText txt0 = let txt1 = tameDescription txt0
                          txt2 = foldl' (\(!txt) clip -> T.replace clip "" txt) txt1 noisyTextClips 
                      in T.strip txt2


preprocessRSSArticle (article,item) =
  let txt1 = cleanNoiseText (item ^. description)
  in if | T.null txt1        -> Nothing
        | T.head txt1 == '*' -> Nothing
        | otherwise          -> let -- this is real extreme ad hoc 
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
    let hsh = article^.rssArticleHash.to bstrHashToB16
        src = article^.rssArticleSource
        txt = item^.description
    fchk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. corenlpstore) </> T.unpack hsh)
    echk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. errstore) </> T.unpack hsh)
    when (not fchk && not echk) $ do
      -- let txt = preprocessText txt0
      eresult <- try $ runParser pp txt
      case eresult of
        Left  (_e :: SomeException) -> do
          saveHashNameTextFileInPrefixSubDirs ((cfg ^. errstore) </> T.unpack hsh) txt
          let err = RSSErrorArticle (b16ToBstrHash hsh) src "" (article^.rssArticleCreated)
          uploadRSSErrorArticleIfMissing conn err
        Right result                -> do
          saveHashNameBSFileInPrefixSubDirs
            ((cfg ^. corenlpstore) </> T.unpack hsh)
            (BL.toStrict $ A.encode result)
          updateRSSAnalysisStatus conn (b16ToBstrHash hsh) (Just True,Nothing,Nothing)
  closeConnection conn



runCoreNLPforRSS :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                 -> PathConfig
                 -> SourceTimeConstraint
                 -> IO ()
runCoreNLPforRSS pp cfg sc = do
  articles <- RSS.getRSSArticleBy cfg sc  
  preParseRSSArticles pp cfg articles
