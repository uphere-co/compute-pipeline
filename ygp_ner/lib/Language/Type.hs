{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Type where

import           Data.Text                 (Text)

type RawText = Text
type TWord = Text
type Sentence = [TWord]
type Paragraph = [Sentence]
type Article = [Paragraph]
