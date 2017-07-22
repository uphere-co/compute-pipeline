{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Source.NYT.Article where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Database.PostgreSQL.Simple as PGS
import           Data.Maybe          (fromJust)
import           Control.Monad
import qualified Data.ByteString.Base16     as B16
import qualified NYT.DB.Article as A
import qualified NYT.DB.Analysis as Analysis
import           NYT.DB
import           NYT.Parser
import           NYT.Type     (NYTArticleFullContent(..))
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Binary                as Bi
import           System.Directory           (listDirectory,createDirectoryIfMissing,doesFileExist)

getAllParsedNYTArticle = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  articles <- getArticleAll conn
  result <- flip mapM articles $ \x -> do
    let hsh = (L8.unpack . L8.fromStrict . B16.encode . A._sha256) x
        fileprefix = "/data/groups/uphere/news-archive/fetchfin/nyt/NYTArticles/"
        filepath = fileprefix ++ hsh ++ ".info/" ++ hsh ++ ".parsed"
    fchk <- doesFileExist filepath
    case fchk of
      True -> do
        (file :: NYTArticleFullContent) <- fmap (Bi.decode . BL.fromStrict) $ B.readFile filepath
        return $ (hsh, file)
      False -> print hsh >> error "error"
  PGS.close conn
  return result
  
getAllAnalyzedNYTArticle = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  analyses <- getAnalysisAll conn
  PGS.close conn
  return $ map (L8.unpack . L8.fromStrict . B16.encode . Analysis._sha256) analyses

