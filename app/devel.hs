{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Language.Java         as J
import qualified Data.ByteString.Char8 as B
import System.Environment              (getArgs,getEnv)
import           Data.Default
--
import           CoreNLP.Simple
import           CoreNLP.Simple.Type
import           TimeTagger.TemporalExpression
--

main :: IO ()
main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    return ()
    {-
    let tt = "This week, this week nothing nothing this week!! Also next week!"
    let tt' = "S&P Global Ratings is proving to be a better predictor of U.S. partisan political discord than an adjudicator of creditworthiness in the eyes of the bond market."
    print tt'
    getTemporalExp tt' pp >>= print
    getWikiEL tt' pp >>= print
    getConstruction tt' pp >>= print
    -}
  -- (n :: Int) <- (read . (!! 0)) <$> getArgs
  -- runTokenizer n

-- runWikiEL -- print =<< getPBFull "Trump canceled Paris."
