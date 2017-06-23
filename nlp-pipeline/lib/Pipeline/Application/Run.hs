{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Application.Run where

import           Control.Lens                    ((^.),_2,_3,_4)
import           Control.Monad                   (forM,forM_)
import qualified Data.ByteString.Char8  as B
import           Data.Text                       (Text)
import qualified Data.Text              as T
import           Data.Text.Read                  (decimal)
import           Language.Java          as J
import           System.Environment              (getEnv)
--
import           CoreNLP.Simple                  (annotate,prepare)
import           CoreNLP.Simple.Type             (PipelineConfig(PPConfig))
import           CoreNLP.Simple.Util
import           PropBank
import           WordNet.Type
--
import           Pipeline.Source.NewsAPI.Article
import           Pipeline.View.YAML.YAYAML()
import           Pipeline.Util
import           Pipeline.Run

run :: IO ()
run = do
  clspath <- getEnv "CLASSPATH"
  filelist <- getFileList "/data/groups/uphere/intrinio/Articles/bloomberg"
  db <- loadDB "/data/groups/uphere/data/NLP/dict"
  pdb <- constructPredicateDB <$> constructFrameDB "/data/groups/uphere/data/NLP/frames"
  let rdb = constructRoleSetDB pdb
    
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True True True False False False)
    forM_ (take 10000 filelist) $ \a' -> do
      txt <- getDescription a'
      doc <- getDoc txt
      ann <- annotate pp doc
      pdoc <- getProtoDoc ann
      let psents = getProtoSents pdoc
          tokens = getAllTokens psents
      (_,ppr) <- getPPR (T.unpack $ mkUkbTextInput (mkUkbInput tokens))
      result <- forM ppr $ \x -> do
        let Right (n,_) = decimal (T.pack (B.unpack $ (x ^. _3)))
        let word = T.pack $ B.unpack $ (x ^. _4)
        let concept = getQueryConcept n (extractPOS $ T.pack $ B.unpack $ (x ^. _2)) db
        let sense = getQuerySense word n db
        case sense of
          Nothing -> print ("" :: String)
          Just s  -> print ("sense : " :: String) >> print s
        let (xs,_) = case concept of
              Nothing -> ([],"")
              Just c  -> c
        flip mapM_ xs $ \x' -> do
          print $ T.intercalate "" [_lex_word x',".",T.pack (show $ _lex_id x')]
          queryRoleSet rdb (T.intercalate "" [_lex_word x',".",T.pack (show $ _lex_id x')])
        return $ (T.pack $ B.unpack $ (x ^. _4))
      putStrLn $ show (txt,result)
  putStrLn "Program is finished!"


runWikiEL :: IO ()
runWikiEL = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (PPConfig True True True False False False False True)
    getWikiEL test_text pp


test_text :: Text
test_text = "United Airlines (UAL.N) and its chief executive faced mounting pressure on Tuesday from a worldwide backlash over its treatment of a passenger who was dragged from his seat on a plane on Sunday to make room for four employees on the overbooked flight."
