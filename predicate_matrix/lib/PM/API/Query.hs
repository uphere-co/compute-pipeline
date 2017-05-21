{-# LANGUAGE OverloadedStrings   #-}

module PM.API.Query where

import           Data.List           (foldl')
import qualified Data.Map     as M
import qualified Data.Set     as S
import Data.Text       (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import PM.Type
import PM.Util

loadPM :: FilePath -> IO [PredicateMatrix]
loadPM fp = do
  txt <- TIO.readFile fp
  let lines = drop 1 $ T.lines txt
      items = map T.words lines
  
  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat

  return enmat

  
createPM :: [PredicateMatrix] -> M.Map Text [(Text,Text)]
createPM mat = fmap (map (\x -> (pbRoleset x, pbArg x))) $ foldl' (\acc x -> M.insertWith' (++) (mcrIliOffset x) [x] acc) M.empty mat

query :: Text -> M.Map Text [(Text,Text)] -> Maybe [(Text,Text)]
query txt pm = M.lookup (T.append "mcr:ili-30-" txt) pm
