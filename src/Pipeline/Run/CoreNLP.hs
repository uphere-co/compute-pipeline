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
import           Control.Lens                                 ((^.),_1,_2,set)
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
import           NLP.Shared.Type                              (ItemRSS,PathConfig,corenlpstore,dbstring,errstore,description)

import           DB.Operation
import qualified DB.Schema.RSS.Article                 as RAr
import           DB.Type                                      (RSSErrorArticleDB(..))
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

-- filterArticle :: * -> *


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

{- 
testFilter = filter (\x-> x^._2._1 `elem`
                            [ "066da3a1ad30aa1525071e9d556c0257895d6e54112d55e919a1e151eacc7ded"
                            , "fff98b05ca41700b439ffedf461fe7516f6fdd00e2b92de90cae1629d2ac5bf4"

                            , "0bc334f992db52211f0dec3038c02f5fdaa5656bc0f74d2a1636686a45901989"
                            , "47a6f3c53c358897a6b58d9b532a51e99e942645e487f937f0a611ec281e1251"
                            , "22320b1b4334597048c7903223bf03ec6a5cd2d031c537f05b0ca650af0ce5e7"
                            , "faf226eadeb958753a6c5be5b46b2b811077bacef96ece006f19c9792b365458"
                            ]
                    )
-}

testFilter = id



preParseRSSArticles :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                       -> PathConfig
                       -> [Maybe (RAr.RSSArticleH,ItemRSS)]
                       -> IO ()
preParseRSSArticles pp cfg articles = do
  conn <- getConnection (cfg ^. dbstring)
  forM_ (testFilter (mapMaybe (join . fmap preprocessRSSArticle) articles)) $ \(article,item) -> do
    let hsh = bstrHashToB16 $ RAr._hash article
        src = T.unpack $ RAr._source article
        txt = item^.description
    fchk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. corenlpstore) </> hsh)
    echk <- doesHashNameFileExistInPrefixSubDirs ((cfg ^. errstore) </> hsh)
    when (not fchk && not echk) $ do
      -- let txt = preprocessText txt0
      eresult <- try $ runParser pp txt
      case eresult of
        Left  (_e :: SomeException) -> do
          saveHashNameTextFileInPrefixSubDirs ((cfg ^. errstore) </> hsh) txt
          uploadRSSErrorArticleIfMissing conn (RSSErrorArticleDB (b16ToBstrHash hsh) (T.pack src) "" (RAr._created article))
        Right result                -> do
          saveHashNameBSFileInPrefixSubDirs ((cfg ^. corenlpstore) </> hsh) (BL.toStrict $ A.encode result)
          updateRSSAnalysisStatus conn (b16ToBstrHash hsh) (Just True,Nothing,Nothing)
  closeConnection conn



runCoreNLPforRSS :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline") -> PathConfig -> SourceConstraint -> IO ()
runCoreNLPforRSS pp cfg sc = do
  articles <- RSS.getRSSArticleBy cfg sc
  preParseRSSArticles pp cfg articles
