module Pipeline.App.RSSMonitor where

import           Control.Lens    ((^.))
import           Control.Monad   (forM_)
import qualified Data.Text as T
--
import           NER
import           NLP.Shared.Type (ItemRSS(..),description,link,pubDate,title)
import           RSS.Load
--
printAll cfg = do
  nt <- loadNameTable 
  companies <- getCompanyList nt

  items <- loadAllRSSItems cfg


  let f i = T.toLower (i ^. description)
      citems = filter (\i -> any (\x -> T.toLower x `T.isInfixOf` (f i)) companies) items

  forM_ citems $ \citem -> do
    print (citem ^. description)
