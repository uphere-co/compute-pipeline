{-# LANGUAGE OverloadedStrings   #-}


module PM.API.Query where

import           Control.Applicative
import           Data.List           (foldl')
import qualified Data.Map     as M
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import PM.Type
import PM.Util

loadPMData :: FilePath -> IO [PredicateMatrix]
loadPMData fp = do
  txt <- TIO.readFile fp
  let lines = drop 1 $ T.lines txt
      items = map T.words lines
  let totalmat = map (\x -> mkPred x) items
      enmat = filter (\x -> idLang x == Eng) totalmat
  return enmat

createPM :: [PredicateMatrix] -> M.Map Text [LinkNet]
createPM mat = fmap (map (\x -> ( PropBank (pbRoleset x)  (pbArg x)
                                , VerbNet (vnClass x)  (vnClassNumber x)  (vnSubclass x)  (vnSubclassNumber x)  (vnLema x)  (vnRole x)
                                , FrameNet (fnFrame x)  (fnLe x)  (fnFrameElement x)
                                , ESO (esoClass x)  (esoRole x) )
                         )) $
  foldl' (\acc x -> M.insertWith' (++) (mcrIliOffset x) [x] acc) M.empty mat

loadPM fp = do
  pmdata <- loadPMData fp
  let pm = createPM pmdata
  return pm

getQueryPM :: Text -> M.Map Text [LinkNet] -> Maybe [LinkNet]
getQueryPM txt pm = M.lookup (T.append "mcr:ili-30-" txt) pm
