{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Default
import           Data.Function                (on)
import           Data.List                    (sort,sortBy)
import           Data.Maybe                   (catMaybes,mapMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text.IO          as TIO
import           Language.Java           as J
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           NLP.Type.PennTreebankII
import           NewsAPI.Type (SourceArticles(..))

printFormat (time,title,desc) = do
  TIO.putStrLn time
  TIO.putStrLn title
  TIO.putStrLn desc
  TIO.putStrLn ""


getTimeTitleDesc :: FilePath -> IO (Maybe (Text,Text,Text))
getTimeTitleDesc fp = do
  bstr <- B.readFile fp 
  let esrc = eitherDecodeStrict bstr :: Either String SourceArticles
  case esrc of
    Left err -> return Nothing
    Right src -> return ((,,) <$> _publishedAt src <*> _title src <*> _description src)




convertToken' :: TK.Token -> Maybe Token
convertToken' t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8' <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8' <$> (t^.TK.pos)
  l <- cutf8' <$> (t^.TK.lemma)
  return (Token (b,e) w p l)


formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))
  
main :: IO ()
main = do
  
  let dir = "/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg"
  cnts <- getDirectoryContents dir
  let cnts' = take 10 $ (filter (\x -> x /= "." && x /= "..")) cnts
  lst <- flip mapM cnts' $ \fp -> getTimeTitleDesc (dir </> fp)
  let ordered = sortBy (compare `on` (^._1)) $ catMaybes lst 


  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                  )
    let (_,title,desc) = head ordered
    doc <- getDoc desc
    ann <- annotate pp doc
    pdoc <- getProtoDoc ann
    print doc
    print pdoc
    let psents = getProtoSents pdoc
        sents = map (convertSentence pdoc) psents
        tktokss = map (getTKTokens) psents
        tokss = map (mapMaybe convertToken') tktokss
    --     tokens = getAllTokens psents
    -- mapM_ print sents -- tokens
    -- mapM_ print tktokss
    mapM_ (mapM_ (putStrLn.formatLemmaPOS)
                 . filter (\t -> isVerb (t^.token_pos))) tokss
  -- mapM_ printFormat $ sortBy (compare `on` (^._1)) $ catMaybes lst 
