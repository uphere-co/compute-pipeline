module Pipeline.Source.NYT.Article where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Database.PostgreSQL.Simple as PGS
import           Data.Maybe          (fromJust)
import           Control.Monad
import qualified Data.ByteString.Base16     as B16
import qualified NYT.DB.Article as A
import           NYT.DB
import           NYT.Parser
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Binary                as Bi
import           System.Directory           (listDirectory,createDirectoryIfMissing,doesFileExist)


getAllNYTArticle = do
  let dbconfig  = L8.toStrict . L8.pack $ "dbname=mydb host=localhost port=65432 user=modori"
  conn <- PGS.connectPostgreSQL dbconfig
  result <- getArticleAll conn
  flip mapM_ result $ \x -> do
    print $ A._id x
    let hsh = (L8.unpack . L8.fromStrict . B16.encode . A._sha256) x
        fileprefix = "/data/groups/uphere/news-archive/fetchfin/nyt/NYTArticles/"
    fchk <- doesFileExist (fileprefix ++ hsh)
    case fchk of
      True -> do
        createDirectoryIfMissing True (fileprefix ++ hsh ++ ".info")
        fchk2 <- doesFileExist (fileprefix ++ hsh ++ ".info/" ++ hsh ++ ".parsed")
        if (not fchk2)
          then do
          txt <- TIO.readFile (fileprefix ++ hsh)
          let pnyt = fromJust $ parseNYTFull txt
          BL.writeFile (fileprefix ++ hsh ++ ".info/" ++ hsh ++ ".parsed") (Bi.encode pnyt)
          else return ()
      False -> print hsh >> error "error"
