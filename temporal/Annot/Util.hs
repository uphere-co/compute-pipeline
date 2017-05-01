module Annot.Util where

import           Data.Maybe (isJust)
--
import           Type
import           Util.Doc
import           View

annotText tagged txt = do
      let ann = (AnnotText . map (\(t,m)->(t,isJust m)) . tagText tagged) txt
          xss = lineSplitAnnot 80 ann
      sequence_ (concatMap (map cutePrintAnnot) xss)
