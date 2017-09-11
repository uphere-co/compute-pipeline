{-# LANGUAGE RecordWildCards   #-}

module Pipeline.Run where

import           Control.Lens
import           Control.Monad                          (forM_)
import           Data.Char                              (isSpace)
import qualified Data.Text as T
import           System.FilePath                        ((</>))
--
import           MWE.Util                               (mkTextFromToken)
import           SRL.Analyze.Type

--
import           Pipeline.Source.NewsAPI.Article        (getTitle)

generateTextMG mg filename (i,sstr,mtks,mg') = do
  atctitle <- fmap (T.unpack . (T.dropWhile isSpace)) $ getTitle ("/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg" </> filename)
  let vertices = mg ^. mg_vertices
      edges = mg ^. mg_edges  
  putStrLn "======================================================================================="
  putStrLn ("filename : " ++ filename)
  putStrLn ("title    : " ++ atctitle)
  putStrLn ("descrip  : " ++ (T.unpack $ T.dropWhile isSpace $ mkTextFromToken mtks))
  forM_ vertices $ \v -> do
    case v of
      MGPredicate {..} -> putStrLn $ "MGPredicate :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_frame)) ++ "    " ++ (T.unpack $ v ^. mv_verb . _1)
      MGEntity    {..} -> putStrLn $ "MGEntity    :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_text))
  forM_ edges $ \e -> do
    putStrLn $ "MGEdge       :  " ++ (T.unpack (e ^. me_relation)) ++ "    " ++  (show $ e ^. me_start) ++ "    "  ++ (show $ e ^. me_end)
  putStrLn "=======================================================================================\n"


          

