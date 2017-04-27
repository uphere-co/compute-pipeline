{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative                  ((<$>),(<*>),(<|>),pure)
import           Control.Concurrent                   (threadDelay)
import           Control.Exception                    (SomeException(..),evaluate,try)
import           Control.Lens
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Identity
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types                     (typeMismatch)
import qualified Data.ByteString.Base16     as B16
import           Data.ByteString.Char8                (ByteString) 
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                          (partitionEithers)
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.List                            (concat,foldl')
import           Data.Text (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding                   (decodeUtf8,encodeUtf8)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as PGS

import           Opaleye                       hiding (constant)
import qualified Opaleye.PGTypes               as P
import           Options.Applicative
import qualified Options.Applicative           as OA
import           System.Environment                   (getArgs)
import           System.IO.Unsafe                     (unsafePerformIO)
-- 
import           Model.Opaleye
import           Model.Opaleye.ShowConstant           (constant)
import           Model.Opaleye.To
import           Model.News.Article
                   (Article
                   ,ArticleP(Article)
                   ,ArticleH)
import qualified Model.News.Article as A

import           Model.News.Author
                   (Author
                   ,AuthorP(Author)
                   ,AuthorH)
import qualified Model.News.Author as Au

import           Model.News.Tag
                   (Tag
                   ,TagP(Tag)
                   ,TagH)
import qualified Model.News.Tag as T

data NYTMeta = NYTMeta
               { _id :: Text
               , _tag :: [Text]
               , _modified :: [Text]
               , _top_level_section :: [Text]
               , _modified_time :: [Text]
               , _published_time :: [Text]
               , _section :: [Text]
               , _section_taxonomy_id :: [Text]
               , _collection :: [Text]
               , _author :: [Text]
               , _section_url :: [Text]
               , _published :: [Text]
               } deriving Show

instance FromJSON NYTMeta where
  parseJSON (Object o) =
    NYTMeta <$> o .: "id"
            <*> (o .: "article:tag"                 <|> pure [])
            <*> (o .: "article:modified"            <|> pure [])
            <*> (o .: "article:top-level-section"   <|> pure [])
            <*> (o .: "article:modified_time"       <|> pure [])
            <*> (o .: "article:published_time"      <|> pure [])
            <*> (o .: "article:section"             <|> pure [])
            <*> (o .: "article:section-taxonomy-id" <|> pure [])
            <*> (o .: "article:collection"          <|> pure [])
            <*> (o .: "article:author"              <|> pure [])
            <*> (o .: "article:section_url"         <|> pure [])
            <*> (o .: "article:published"           <|> pure [])
  parseJSON invalid = typeMismatch "NYTMeta" invalid

data NYTURL = NYTURL
              { nyturl_id :: Text
              , nyturl_url :: Text
              } deriving Show


data NYTArticle = NYTArticle
                  { article_id :: ByteString
                  , article_id_base16 :: Text
                  , article_url :: Text
                  , article_author :: [Text]
                  , article_modified :: Maybe UTCTime
                  , article_published :: Maybe UTCTime
                  , article_top_level_section :: Maybe Text
                  , article_section :: Maybe Text
                  , article_section_taxonomy_id :: Maybe Text
                  , article_section_url :: Maybe Text
                  , article_collection :: Maybe Text
                  , article_tag :: [Text]
                  } deriving Show




data NYTOption = NYTOption { _metaJson :: String
                           , _hashUrl  :: String
                           , _db       :: String
                           , _host     :: String
                           , _port     :: String
                           , _user     :: String
                           }

pOptions :: Parser NYTOption
pOptions = NYTOption <$> OA.argument str (metavar "json")
           <*> OA.argument str (metavar "hashurl")
           <*> strOption (long "db" <> short 'd' <> help "DB name")
           <*> strOption (long "host" <> short 'h' <> help "Host name")
           <*> strOption (long "port" <> short 'p' <> help "Port number")
           <*> strOption (long "user" <> short 'u' <> help "User name")

nytOption = info pOptions ( fullDesc <> progDesc "NYT DB sync App" <> header "options are meta JSON file, hash-url file, DB name, host name, port number and user name.")


readT :: Text -> Either SomeException UTCTime
readT txt =
  let x = runIdentity $ parseTimeM True defaultTimeLocale "%Y-%m-%dT%T%z" (T.unpack txt)
  in unsafePerformIO $ try (evaluate x)

      

mkNYTArticle :: NYTMeta -> HM.HashMap Text Text -> Either [(NYTMeta,Maybe Text,String)] NYTArticle
mkNYTArticle n@NYTMeta {..} m = 
  case HM.lookup _id m of
    Nothing -> Left [(n,Nothing,"no url")]
    Just url -> runIdentity $ runEitherT $ do
      let eitherHead (x:xs) = right x
          eitherHead []     = left [(n,Just url,"no element")]
          readT' t = mapEitherT (fmap f) (hoistEither (readT t))
            where f (Left ex) = Left [(n,Just url,show ex)]
                  f (Right b)  = Right b
          
      mtime <- ((fmap Just . readT' =<< eitherHead (if Prelude.null _modified_time
                                                    then _modified
                                                    else _modified_time))
                <|> return Nothing)
      ptime <- ((fmap Just . readT' =<< eitherHead (if Prelude.null _published_time
                                                    then _published
                                                    else _published_time))
                <|> return Nothing)
      tsec     <- (fmap Just (eitherHead _top_level_section) <|> return Nothing)
      sec      <- (fmap Just (eitherHead _section) <|> return Nothing)
      sectaxid <- (fmap Just (eitherHead _section_taxonomy_id) <|> return Nothing)
      securl   <- (fmap Just (eitherHead _section_url) <|> return Nothing)
      coll     <- (fmap Just (eitherHead _collection) <|> return Nothing)

      let idbstr = fst (B16.decode (encodeUtf8 _id))
          r = NYTArticle
              { article_id = idbstr
              , article_id_base16 = _id
              , article_url = url
              , article_author = _author
              , article_modified = mtime
              , article_published = ptime
              , article_top_level_section = tsec
              , article_section = sec
              , article_section_taxonomy_id = sectaxid
              , article_section_url = securl
              , article_collection = coll
              , article_tag = _tag
              }
      return r




mkPairs :: Text -> [NYTURL]
mkPairs txt = let ls = T.lines txt
                  f x = let h:u:_ = T.words x
                        in NYTURL h u
              in map f ls


mkHashURLMap :: FilePath -> IO (HM.HashMap Text Text)
mkHashURLMap fp = do
  txt <- TIO.readFile fp 
  let ps = mkPairs txt
  return $ foldl' (\acc (NYTURL k v) -> HM.insert k v acc) HM.empty ps



uploadArticle :: PGS.Connection -> NYTArticle -> IO ()
uploadArticle conn NYTArticle {..} = do
  runInsert conn A.table $
    A.newArticle article_id article_url article_modified article_published
      article_top_level_section article_section article_section_url
      article_section_taxonomy_id article_collection
  runInsertMany conn Au.table (map (Au.newAuthor article_id) article_author)
  runInsertMany conn T.table (map (T.newTag article_id) article_tag)
  return ()


updateArticle :: PGS.Connection -> NYTArticle -> IO ()
updateArticle conn NYTArticle {..} = do
  runUpdate conn A.table (\(A.Article i _ _ _ _ _ _ _ _ _)  -> (A.newArticle article_id article_url article_modified article_published
    article_top_level_section article_section article_section_url article_section_taxonomy_id article_collection) {A._id = Just i})
    (\x -> (A._sha256 x) .== (constant article_id))
  -- TO-DO : Implement runUpdateMany
  -- Update policy for authors and tags will be determined later.
  -- runInsertMany conn Au.table (map (Au.newAuthor article_id) article_author)
  -- runInsertMany conn T.table (map (T.newTag article_id) article_tag)
  return ()


updateArticleDB :: PGS.Connection -> NYTArticle -> IO ([Text])
updateArticleDB conn NYTArticle {..} = do
  (lst :: [ByteString]) <- runUpdateReturning conn A.table (\(A.Article i _ _ _ _ _ _ _ _ _)  -> (A.newArticle article_id article_url article_modified article_published
    article_top_level_section article_section article_section_url article_section_taxonomy_id article_collection) {A._id = Just i})
    (\x -> (A._sha256 x) .== (constant article_id))
    (A._sha256)
  -- TO-DO : Implement runUpdateMany
  -- Update policy for authors and tags will be determined later.
  -- runInsertMany conn Au.table (map (Au.newAuthor article_id) article_author)
  -- runInsertMany conn T.table (map (T.newTag article_id) article_tag)
  -- print (map B16.encode lst)
  return (map (decodeUtf8 . B16.encode) lst)

getRemaining rs updated = filter (\x -> Prelude.not $ (article_id_base16 x) `elem` updated) rs

main :: IO ()
main = do
  opt <- execParser nytOption
  str <- BL.readFile (_metaJson opt)
  m <- mkHashURLMap (_hashUrl opt)
  let db   = _db opt
      host = _host opt
      port = _port opt
      user = _user opt
      
  let ev :: Either String [NYTMeta] = eitherDecode str
  case ev of
    Left err -> print err
    Right vs -> do
      let lst = map (\v -> mkNYTArticle v m) vs
          (ls,rs) = partitionEithers lst

      -- print (length ls, length rs)
      -- mapM_ print ls

      let bstr  = BL.toStrict . BL.pack $ "dbname=" ++ db ++ " host="++ host ++ " port="++ port ++ " user=" ++ user
      conn <- PGS.connectPostgreSQL bstr
      
      -- mapM_ (uploadArticle conn) rs
      -- mapM_ (updateArticle conn) rs -- for update
      updated <- fmap concat $ mapM (updateArticleDB conn) rs
      let remaining = getRemaining rs updated
      mapM_ (uploadArticle conn) remaining

      putStrLn ("Total number of article : " ++ (show (length rs)))
      putStrLn ("Number of updated article : " ++ (show (length updated)))
      putStrLn ("Number of uploaded article : " ++ (show (length remaining)))

      PGS.close conn
