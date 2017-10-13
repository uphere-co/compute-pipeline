{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Source.NYT.Article where

import           Control.Lens                      ((^.))
import qualified Data.Binary                as Bi
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Database.PostgreSQL.Simple as PGS
import           System.Directory                  (doesFileExist)
import           System.FilePath                   ((</>))
--
import           NLP.Shared.Type                   (PathConfig,dbstring,nytstore)
-- import           NYT.DB
-- import           RSS.Source.NYT.DB
import           DB.Operation                      (getRSSArticleAll,getRSSAnalysisAll)
import qualified DB.Schema.RSS.Analysis     as Analysis
import qualified DB.Schema.RSS.Article      as A
import           RSS.Type                          (NYTArticleFullContent(..))
--
import           Pipeline.Type

getAllParsedNYTArticle :: PathConfig -> IO [(String, NYTArticleFullContent)]
getAllParsedNYTArticle cfg = do
  let dbconfig  = L8.toStrict . L8.pack $ (cfg ^. dbstring)
  conn <- PGS.connectPostgreSQL dbconfig
  articles <- getRSSArticleAll conn
  result <- flip mapM articles $ \x -> do
    let hsh = (L8.unpack . L8.fromStrict . B16.encode . A._hash) x
        fileprefix = (cfg ^. nytstore)
        filepath = (fileprefix </> hsh) ++ ".info/" ++ hsh ++ ".parsed"
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        (file :: NYTArticleFullContent) <- fmap (Bi.decode . BL.fromStrict) $ B.readFile filepath
        return $ (hsh, file)
      False -> print hsh >> error "error"
  PGS.close conn
  return result

getAllAnalyzedNYTArticle :: PathConfig -> IO [String]
getAllAnalyzedNYTArticle cfg = do
  let dbconfig  = L8.toStrict . L8.pack $ (cfg ^. dbstring)
  conn <- PGS.connectPostgreSQL dbconfig
  analyses <- getRSSAnalysisAll conn
  PGS.close conn
  return $ map (L8.unpack . L8.fromStrict . B16.encode . Analysis._hash) analyses

