{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.Source.NYT.Article where

import qualified Data.Binary                as Bi
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Database.PostgreSQL.Simple as PGS
import           System.Directory                  (doesFileExist)
--
import           NYT.DB
import qualified NYT.DB.Analysis            as Analysis
import qualified NYT.DB.Article             as A
import           NYT.Type                          (NYTArticleFullContent(..))


getAllParsedNYTArticle :: IO [(String, NYTArticleFullContent)]
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

getAllAnalyzedNYTArticle :: IO [String]
getAllAnalyzedNYTArticle = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  analyses <- getAnalysisAll conn
  PGS.close conn
  return $ map (L8.unpack . L8.fromStrict . B16.encode . Analysis._sha256) analyses

