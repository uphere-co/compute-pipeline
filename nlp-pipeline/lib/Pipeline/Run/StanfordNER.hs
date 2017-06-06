{-# LANGUAGE OverloadedStrings #-}

module Pipeline.Run.StanfordNER where

import           CoreNLP.Simple.Type.Simplified
--

fromNERSentence :: NERSentence -> [NERToken]
fromNERSentence (NERSentence xs) = xs
