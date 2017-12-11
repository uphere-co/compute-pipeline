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
import           Data.List                      (foldl')
import           Data.Maybe                     (catMaybes)
import           Data.Text                      (Text)
import qualified Data.Text                  as T
import qualified Language.Java              as J
import           System.Environment             (getEnv)
--
import           CoreNLP.Simple                 (prepare)
import           CoreNLP.Simple.Type            (constituency,lemma,ner,postagger,sutime,tokenizer,words2sentences)
import           EventAnalyzer.Classification   (identifyCommoditiesEvent)
import           Lexicon.Data                   (loadLexDataConfig)
import           NER
import           NER.Load
import           NER.Type
import           NLP.Shared.Type                (EventClass(..),ItemRSS(..),PathConfig,dbstring,description,lexconfigpath,link,pubDate,title)
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
--  cfgG <- (\ec -> case ec of {Left err -> error err;Right cfg -> return cfg;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
--  (apredata,netagger,forest) <- loadConfig False cfgG
  items <- fmap (take 10000) $ loadAllRSSItems cfg
--  let totaln = length items

  let ecs = map (\x -> identifyCommoditiesEvent (tameDescription $ x ^. description)) items

  let stat = let n1 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "OilDemand&Supply")) x) then (acc+1) else acc) 0 ecs
                 n2 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "Tanker")          ) x) then (acc+1) else acc) 0 ecs
                 n3 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "StockMovement")   ) x) then (acc+1) else acc) 0 ecs
                 n4 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "OilTrade")        ) x) then (acc+1) else acc) 0 ecs
                 n5 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "ShaleOil")        ) x) then (acc+1) else acc) 0 ecs
                 n6 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "MarketData")      ) x) then (acc+1) else acc) 0 ecs
                 n7 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Energy" (Just "OPEC")            ) x) then (acc+1) else acc) 0 ecs
                 n8 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Soybean" Nothing                 ) x) then (acc+1) else acc) 0 ecs
                 n9 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Oats" Nothing                    ) x) then (acc+1) else acc) 0 ecs
                 n10 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Rice" Nothing                   ) x) then (acc+1) else acc) 0 ecs
                 n11 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Wheat" Nothing                  ) x) then (acc+1) else acc) 0 ecs
                 n12 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Palm Oil" Nothing               ) x) then (acc+1) else acc) 0 ecs
                 n13 = foldl' (\acc x -> if (any (== EventClass "Commodities" "Corn" Nothing                   ) x) then (acc+1) else acc) 0 ecs
             in (n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13)

  print stat
      {-
                    
  forM_ (zip [1..] items) $ \(i,item) -> do
    let txt = tameDescription $ item ^. description

        txts = tokenizeText txt
        s = runState (runEitherT (many $ pTreeAdvG forest)) txts

--    eprst <- try $ runParser pp (tameDescription $ item ^. description) 
    let ec = identifyCommoditiesEvent txt
    when (((not . null) ec) && (T.length txt < 450)) $ do
      print (ec,txt)

-}













    {-
    if (isRight (fst s))
      then do
      let Right s' = fst s
      if (length s' > 0)
        then do
        if (["Royal","Dutch","Shell"] `elem` s')
          then print ((show i) ++ "/" ++ (show totaln),s',txts)
          else return ()
--        print "Matched"
--        print (s',txts)
--        _ <- getChar
        return (Just item)
        else do
--        print "Not Matched"
--        print (txts)
--        _ <- getChar
        return Nothing
      else return Nothing
    -}

--  print (length $ catMaybes mlist)
{-      wlsts <- case eprst of
        Left (e :: SomeException) -> return []
        Right prst -> getWikiList apredata netagger forest prst
      if (((length s') + (length wlsts)) > 0)
        then return (Just "Matched")
        else return Nothing -}



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
