module Storage.Util where

import Storage.Type

-- |
checkUrl :: String -> Maybe URLtype
checkUrl str =
  if length str > 6
  then let (method,path)= splitAt 7 str
       in case method of
            "file://" -> Just (LocalURL path)
            "http://" -> Just (GlobalURL str)
            _ -> Nothing
  else Nothing
