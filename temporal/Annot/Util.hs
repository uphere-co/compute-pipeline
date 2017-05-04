module Annot.Util where

import           Data.Maybe (isJust)
import           Data.Text  (Text)
--
import           Type       (AnnotText(..))
import           Util.Doc   (tagText)
import           View       (cutePrintAnnot,lineSplitAnnot)

annotText :: [(a,Int,Int)] -> Text -> IO ()
annotText tagged txt = do
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
