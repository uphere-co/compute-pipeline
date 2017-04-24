{-# LANGUAGE OverloadedStrings #-}

module Pattern where

import           Data.Text              (Text)
--
import           Type
import           Util

splitNumber :: Text -> [Text]
splitNumber txt = filter (not . isInteger) (sepBy "0123456789" txt)

-- splitNumber :: Text -> [Text]
-- splitNumber = split (compile "\\d+" []) 
-- "No 3 Article" -> "N

-- You should specify the type what you want to check to use this function.
-- Then function will check if the given text is the specified type.
checkTyp :: Token -> Text -> Bool
checkTyp typ txt = case typ of
  -- TWord _  -> isWord txt
  Number    -> isContainingNumber txt
  _         -> undefined
  -- Period   -> isPeriod txt
  -- Comma    -> isComma txt
