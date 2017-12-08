{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.RSSMonitor where

import           Control.Applicative            (many)
import           Control.Exception              (SomeException,try)
import           Control.Lens                   ((&),(^.),(.~))
import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either     (EitherT(..))
import qualified Data.ByteString.Char8      as B
import           Data.Char                      (isSpace)
import           Data.Default                   (def)
import           Data.Either                    (isRight)
import           Data.Maybe                     (catMaybes)
import           Data.Text                      (Text)
import qualified Data.Text                  as T
import qualified Language.Java              as J
import           System.Environment             (getEnv)
--
import           CoreNLP.Simple                 (prepare)
import           CoreNLP.Simple.Type            (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           Lexicon.Data                   (loadLexDataConfig)
import           NER
import           NER.Load
import           NER.Type
import           NLP.Shared.Type                (ItemRSS(..),PathConfig,dbstring,description,lexconfigpath,link,pubDate,title)
import           RSS.Load
import           SRL.Analyze                    (loadConfig)
import           SRL.Analyze.CoreNLP            (runParser)
import           SRL.Analyze.SentenceStructure  (docStructure,mkWikiList)
import           SRL.Analyze.Type               (ds_sentStructures)
import           Text.Search.New.ParserCustom       (pTreeAdvG)
--
import           Pipeline.Operation.DB          (closeConnection,getConnection)
import           Pipeline.Run.CoreNLP           (tameDescription)




tokenizeText = T.split (\c -> (isSpace c) || (c == '\8217'))


printAll cfg pp = do
  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger,forest,companyMap) <- loadConfig False cfgG
  items <- fmap (take 10000) $ loadAllRSSItems cfg   -- ? what is this 10000
  mfitems <- forM (zip [1..] items) $ \(i,item) -> do
    print i
    let txts = tokenizeText $ (tameDescription $ item ^. description)
        s = runState (runEitherT (many $ pTreeAdvG forest)) txts
    eprst <- try $ runParser pp (tameDescription $ item ^. description) 

    if (isRight (fst s))
      then do
      let Right s' = fst s
      wlsts <- case eprst of
        Left (e :: SomeException) -> return []
        Right prst -> getWikiList apredata netagger (forest,companyMap) prst
      if (((length s') + (length wlsts)) > 0)
        then return (Just "Matched")
        else return Nothing
      else return Nothing
  print $ length items
  print $ length (catMaybes mfitems)


runWithCoreNLP :: PathConfig -> IO ()
runWithCoreNLP cfg = do
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
    printAll cfg pp
  closeConnection conn

getWikiList apredata netagger (forest,companyMap) prst = do
  dstr <- docStructure apredata netagger (forest,companyMap) prst
  let sstrs = catMaybes (dstr ^. ds_sentStructures)
      wikilsts = map (mkWikiList companyMap) sstrs
  return wikilsts
