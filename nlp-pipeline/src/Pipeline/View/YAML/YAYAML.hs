{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Pipeline.View.YAML.YAYAML where

import           Control.Lens
import qualified Data.Text.Lazy             as TL
import           Data.Text                        (Text)
import qualified Data.Text                  as T


import           NLP.Type.CoreNLP
import           YAML.Builder


instance MakeYaml Int where
  makeYaml _ x = YPrim (YInteger x)

instance MakeYaml (Int,Int) where
  makeYaml n (x,y) = YLArray Inline [ makeYaml n x, makeYaml n y ]

instance MakeYaml SentenceIndex where
  makeYaml n s = YObject [ ("index"     , makeYaml n (s^.sent_index))
                         , ("charRange" , makeYaml n (s^.sent_charRange))
                         , ("tokenRange", makeYaml n (s^.sent_tokenRange)) ]

instance MakeYaml Text where
  makeYaml _ txt = YPrim (YString Plain (TL.fromStrict txt))

instance MakeYaml Token where
    makeYaml n t = YObject [ ("range_tok", makeYaml n (t^.token_tok_idx_range))
                           , ("range_char", makeYaml n (t^.token_char_idx_range))
                           , ("text" , makeYaml n (t^.token_text))
                           , ("pos"  , makeYaml n (T.pack (show (t^.token_pos))))
                           , ("lemma", makeYaml n (t^.token_lemma))
                           ]

instance MakeYaml a => MakeYaml [a] where
  makeYaml n xs = YIArray (map (makeYaml n) xs)
