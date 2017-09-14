{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run where

import           Control.Concurrent.Async               (async,wait)
import           Control.Lens
import           Control.Monad                          (forM_,void)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                              (isSpace)
import qualified Data.Text as T
import           System.Directory                       (withCurrentDirectory)
import           System.FilePath                        ((</>),addExtension)
import           System.Process                         (readProcess)
--
import           MWE.Util                               (mkTextFromToken)
import           NLP.Syntax.Type.Verb
import           SRL.Analyze.Format                     (dotMeaningGraph)
import           SRL.Analyze.Type
import           Text.Format.Dot                        (mkLabelText)
--
import           Pipeline.Run.WikiEL
import           Pipeline.Source.NewsAPI.Article        (getTitle)
import           Pipeline.Util                          (saveHashNameBSFileInPrefixSubDirs)


showTextMG mg filename (i,sstr,mtks,mg') = do
  atctitle <- fmap (T.unpack . (T.dropWhile isSpace)) $ getTitle ("/data/groups/uphere/repo/fetchfin/newsapi/Articles/bloomberg" </> filename)

  let vertices = mg ^. mg_vertices
      edges = mg ^. mg_edges  

  putStrLn "======================================================================================="
  putStrLn ("filename : " ++ filename)
  putStrLn ("title    : " ++ atctitle)
  putStrLn ("descrip  : " ++ (T.unpack $ T.dropWhile isSpace $ mkTextFromToken mtks))

  forM_ vertices $ \v -> do
    case v of
      MGPredicate {..} -> putStrLn $ "MGPredicate :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_frame)) ++ "    " ++ (T.unpack $ T.intercalate " " $ v ^. mv_verb . vp_words ^.. traverse . to (^. _1)) 
      MGEntity    {..} -> putStrLn $ "MGEntity    :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_text))

  forM_ edges $ \e -> do
    putStrLn $ "MGEdge       :  " ++ (T.unpack (e ^. me_relation)) ++ "    " ++  (show $ e ^. me_start) ++ "    "  ++ (show $ e ^. me_end)

  putStrLn "=======================================================================================\n"

genMGFigs savedir i filename mtks mg = do
  let title = mkTextFromToken mtks
      dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
  
  withCurrentDirectory savedir $ do
    s <- async (writeFile (filename ++ "_" ++ (show i) ++ ".dot") dotstr)
    r <- wait s
    void (readProcess "dot" ["-Tpng",filename ++ "_" ++ (show i) ++ ".dot","-o"++ filename ++ "_" ++ (show i) ++ ".png"] "")

saveMG savedir filename mgs = do
  saveHashNameBSFileInPrefixSubDirs (savedir </> (addExtension filename "mgs")) (BL8.toStrict $ A.encode mgs)

saveWikiEL fp wikiel = B.writeFile (fp ++ ".wiki") (BL8.toStrict $ A.encode wikiel)
wikiEL emTagger sents = getWikiResolvedMentions emTagger sents
