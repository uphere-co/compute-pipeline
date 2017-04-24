{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8  as BL
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict         as M
import           Data.List
import           Data.Maybe
--import qualified Data.Map as M
import           Data.Text                         (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding                (decodeUtf8)
import qualified Data.Text.IO                as TIO
import           System.Directory                  ( getCurrentDirectory
                                                   , getDirectoryContents )
import           System.Environment                ( getArgs )
import           System.IO                         ( withFile, IOMode(..) )
import           Text.HTML.TagSoup

getPropCont :: Tag Text -> Maybe (Text,Text)
getPropCont (TagOpen "meta" attrs) =
  (,) <$> lookup "property" attrs <*> lookup "content" attrs

combine1 :: HashMap Text [Text] -> (Text,Text) -> HashMap Text [Text]
combine1 m (k,v) = M.alter u k m
  where u Nothing   = Just [v]
        u (Just vs) = Just (vs++[v])

combine = foldl' combine1 M.empty 

process1 :: FilePath -> Text -> Value
process1 fp str =
  let tags = parseTags str
      metas = filter (\case TagOpen "meta" _ -> True; _ -> False) tags 
      propConts = mapMaybe getPropCont metas
      propMap = combine propConts
      articleMap = M.filterWithKey (\k _ -> T.take 8 k == "article:") propMap
      articleJson = Object (M.insert "id" (String (T.pack fp)) (M.map toJSON articleMap))
  in articleJson


main = do
  cwd <- getCurrentDirectory
  fps <- getDirectoryContents cwd


  let hashes = sort $ filter (\x -> length x == 64) fps 

  withFile "result.json" WriteMode $ \oh -> do
    BL.hPutStrLn oh "["
    singlefile oh (Prelude.head hashes)
    flip Prelude.mapM_ (Prelude.tail hashes) $ \fp -> do
      putStrLn fp
      BL.hPutStrLn oh ","
      singlefile oh fp
    BL.hPutStrLn oh "]"
        
 where process fp = do
         lbs <- sinkLbs
         let txt = decodeUtf8 (BL.toStrict lbs)
         return $! process1 fp txt
       singlefile oh fp = do
         v <- runResourceT $ runConduit $ sourceFile fp =$= process fp
         BL.hPutStrLn oh (encodePretty v)

        
