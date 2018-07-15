{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Run where

import           Control.Monad                          (void)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Encoding         as T.E
import           System.Directory                       (getCurrentDirectory,setCurrentDirectory)
import           System.FilePath                        ((</>),(<.>),addExtension,takeBaseName)
import           System.Process                         (readProcess)
--
import           MWE.Util                               (mkTextFromToken)
import           NLP.Type.CoreNLP                       (Token)
import           SRL.Analyze.Format                     (dotMeaningGraph)
import           SRL.Analyze.Type
import           Text.Format.Dot                        (mkLabelText)
--
import           Pipeline.Util                          (saveHashNameBSFileInPrefixSubDirs,splitPrefixSubDirs)



mkMGDotFigs :: (Show a) => FilePath -> a -> FilePath -> [Maybe Token] -> MeaningGraph -> IO ()
mkMGDotFigs savedir i filename mtks mg = do
  let title = mkTextFromToken mtks
      dottxt = dotMeaningGraph (Just (mkLabelText title)) mg
      filepath = (savedir </> filename) ++ "_" ++ (show i) ++ ".dot"

  saveHashNameBSFileInPrefixSubDirs filepath (T.E.encodeUtf8 dottxt)
  let fname = takeBaseName filepath
  let (_hsh,storepath,prefix) = splitPrefixSubDirs filepath
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
