{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Tokenizer where

import           Data.Maybe                      (mapMaybe)
import           Control.Monad                   (forM_)
import qualified Data.ByteString.Char8  as B
import qualified Data.Text              as T
import           Language.Java          as J
import           System.Environment              (getEnv)
--
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple.Util
import           CoreNLP.Simple.Convert
--
import           Pipeline.Source.NYT.Article
import           Pipeline.Run

runTokenizer :: IO ()
runTokenizer = do
  particles <- getAllParsedNYTArticle
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True False False False True)
    forM_ (take 1 particles) $ \pa -> do      
      let txt = T.intercalate "    " pa
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          tktokss = map (getTKTokens) psents
          tokss = map (mapMaybe convertToken) tktokss
      print tokss
