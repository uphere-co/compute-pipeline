{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Tokenizer where

import           Data.Maybe                      (fromMaybe,mapMaybe)
import           Data.Binary                     (Binary,encode)
import           Control.Monad                   (forM_)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Language.Java          as J
import           System.Environment              (getEnv)
import           GHC.Generics
--
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple.Type.Simplified  (Token(..))
import           CoreNLP.Simple.Util
import           CoreNLP.Simple.Convert
--
import           Pipeline.Source.NYT.Article
import           Pipeline.Run

data TokenizedNYTArticle = TokenizedNYTArticle
  { _tokenizedTitle :: [[Token]]
  , _tokenizedSummary :: [[Token]]
  , _tokenizedMaintext :: [[Token]]
  } deriving (Show, Generic)

instance Binary TokenizedNYTArticle

runTokenizer :: IO ()
runTokenizer = do
  particles <- getAllParsedNYTArticle
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True False False False True)
    forM_ (take 1000 particles) $ \(fp,mtitle',msummary',maintext') -> do      
      let title = T.intercalate "    " (fmap (fromMaybe ("" :: Text)) mtitle')
          summary = T.intercalate "    " (fmap (fromMaybe ("" :: Text)) msummary')
          maintext = T.intercalate "    " maintext'
      
      ttk <- getSimplifiedTokensFromText title pp
      stk <- getSimplifiedTokensFromText summary pp
      mtk <- getSimplifiedTokensFromText maintext pp

      let tokenizednyt = TokenizedNYTArticle
            { _tokenizedTitle = ttk
            , _tokenizedSummary = stk
            , _tokenizedMaintext = mtk }

      BL.writeFile fp (encode tokenizednyt)
      
getSimplifiedTokensFromText txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  return tokss
