{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Tokenizer where
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.IO.Class          (liftIO)
import           Data.Maybe                      (catMaybes,fromMaybe,mapMaybe)
import           Data.Binary                     (Binary,encode)
import           Control.Monad                   (forM_,when,void)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Database.Redis
import           Language.Java          as J
import           System.Environment              (getEnv)
import           GHC.Generics
import qualified NYT.Type               as NYT

import qualified Control.Monad.State    as State
import           Control.Monad.Loops
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

runTokenizer :: Int -> IO ()
runTokenizer n = do
  particles'' <- getAllParsedNYTArticle
  sanalyses <- getAllAnalyzedNYTArticle

  let particles' = filter (\(h,f) -> not (h `elem` sanalyses)) particles''

  conn <- checkedConnect defaultConnectInfo { connectHost = "localhost", connectPort = PortNumber 11111 }
  particles <- runRedis conn $ do
    a' <- flip State.evalStateT 0 $ do
      pa' <- flip takeWhileM particles' $ \(hsh,article) -> do
        m <- State.get
        eb <- lift $ exists (B.pack hsh)
        let Right b = eb
        if b
          then return True
          else do            
          -- liftIO $ print hsh
          if (m+1 > n)
            then return False
            else do
            lift $ setnx (B.pack hsh) ("tokenization" :: ByteString)
            State.put (m+1)
            return True
      return pa'
    return a'

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True False False False True)
    forM_ particles $ \(hsh,article) -> do      
      let title = T.intercalate "    " (fmap (fromMaybe ("" :: Text)) (NYT._title article))
          summary = T.intercalate "    " (fmap (fromMaybe ("" :: Text)) (NYT._summary article))
          maintext = T.intercalate "    " (NYT._maintext article)
      
      ttk <- getSimplifiedTokensFromText title pp
      stk <- getSimplifiedTokensFromText summary pp
      mtk <- getSimplifiedTokensFromText maintext pp

      let tokenizednyt = TokenizedNYTArticle
            { _tokenizedTitle = ttk
            , _tokenizedSummary = stk
            , _tokenizedMaintext = mtk }

      let savepath = "/data/groups/uphere/news-archive/fetchfin/nyt/NYTArticles/" ++  hsh ++ ".info/" ++ hsh ++ ".tokenized"
      print savepath
      -- BL.writeFile savepath (encode tokenizednyt)

  void $ runRedis conn $ do
    del (map (B.pack . fst) particles)
    quit
    
getSimplifiedTokensFromText txt pp = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss
  return tokss
