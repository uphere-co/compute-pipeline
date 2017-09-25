{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run where

import           Control.Lens                    hiding ((<.>))
import           Control.Monad                          (forM_,void)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                              (isSpace)
import qualified Data.Text as T
import           System.Directory                       (getCurrentDirectory,setCurrentDirectory)
import           System.FilePath                        ((</>),(<.>),addExtension,takeBaseName,takeFileName)
import           System.Process                         (readProcess)
--
import           MWE.Util                               (mkTextFromToken)
import           NLP.Shared.Type                        (PathConfig,newsapistore)
import           NLP.Syntax.Type.Verb
import           NLP.Type.CoreNLP                       (Token)
import           SRL.Analyze.Format                     (dotMeaningGraph)
import           SRL.Analyze.Type
import           Text.Format.Dot                        (mkLabelText)
--
import           Pipeline.Source.NewsAPI.Article        (getTitle)
import           Pipeline.Type
import           Pipeline.Util                          (saveHashNameBSFileInPrefixSubDirs,splitPrefixSubDirs)


showTextMG :: PathConfig -> MeaningGraph -> FilePath -> (t2, t1, [Maybe Token], t) -> IO ()
showTextMG cfg mg filename (_i,_sstr,mtks,_mg') = do
  atctitle <- fmap (T.unpack . (T.dropWhile isSpace)) $ getTitle ((cfg ^. newsapistore) </> "bloomberg" </> filename)

  let vertices = mg ^. mg_vertices
      edges = mg ^. mg_edges

  putStrLn "======================================================================================="
  putStrLn ("filename : " ++ filename)
  putStrLn ("title    : " ++ atctitle)
  putStrLn ("descrip  : " ++ (T.unpack $ T.dropWhile isSpace $ mkTextFromToken mtks))

  forM_ vertices $ \v -> do
    case v of
      MGPredicate {..} -> putStrLn $ "MGPredicate :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_frame)) ++ "    " ++ (T.unpack $ T.intercalate " " $ v ^. mv_verb . vp_words ^.. traverse . to (^. _1))
      MGNominalPredicate {..} -> putStrLn $ "MGNominalPredicate :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_frame))
      MGEntity    {..} -> putStrLn $ "MGEntity    :  " ++ (show $ v ^. mv_id) ++ "    " ++ (show (v ^. mv_range)) ++ "    " ++ (T.unpack (v ^. mv_text))

  forM_ edges $ \e -> do
    putStrLn $ "MGEdge       :  " ++ (T.unpack (e ^. me_relation)) ++ "    " ++  (show $ e ^. me_start) ++ "    "  ++ (show $ e ^. me_end)

  putStrLn "=======================================================================================\n"

mkMGDotFigs :: (Show a) => FilePath -> a -> FilePath -> [Maybe Token] -> MeaningGraph -> IO ()
mkMGDotFigs savedir i filename mtks mg = do
  let title = mkTextFromToken mtks
      dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
      filepath = (savedir </> filename) ++ "_" ++ (show i) ++ ".dot"

  saveHashNameBSFileInPrefixSubDirs filepath (B.pack dotstr) -- (BL8.toStrict $ A.encode json)
  -- writeFile (filepath ++ "_" ++ (show i) ++ ".dot") dotstr
  let fname = takeBaseName filepath
  let (hsh,storepath,prefix) = splitPrefixSubDirs filepath
  dir <- getCurrentDirectory
  setCurrentDirectory (storepath </> prefix)
  void $ readProcess "dot" ["-Tpng",fname <.> "dot","-o",fname <.>"png"] ""
  setCurrentDirectory dir


saveJSON :: A.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveJSON savedir filename json = saveHashNameBSFileInPrefixSubDirs (savedir </> filename) (BL8.toStrict $ A.encode json)

saveMGs :: A.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveMGs savedir filename mgs = saveJSON savedir (addExtension filename "mgs") mgs

saveMG :: A.ToJSON a => FilePath -> FilePath -> Int -> a -> IO ()
saveMG savedir filename i mg = saveJSON savedir (addExtension (filename ++ "_" ++ (show i)) "mgs") mg

saveARB :: A.ToJSON a => FilePath -> FilePath -> Int -> a -> IO ()
saveARB savedir filename i arb = saveJSON savedir (addExtension (filename ++ "_" ++ (show i)) "arb") arb

saveWikiEL :: A.ToJSON a => FilePath -> a -> IO ()
saveWikiEL fp wikiel = B.writeFile (fp ++ ".wiki") (BL8.toStrict $ A.encode wikiel)
