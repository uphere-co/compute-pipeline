oneDayArticles :: Connection -> Text -> IO [Ar.RSSArticleH]
oneDayArticles conn txt = do
  ctime <- getCurrentTime
  let yesterday = addUTCTime (-nominalDay) ctime
  articles <- getRSSArticleBySourceAndTime conn (T.unpack txt) yesterday
  return articles


getOneDayArticles :: Connection -> Text -> IO [(Int,Text,Text)]
getOneDayArticles conn txt = do
  articles <- oneDayArticles conn txt
  let aList = map (\x -> (Ar._id x, T.pack $ bstrHashToB16 $ Ar._hash x, Ar._source x)) articles
  return aList


nDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [An.RSSAnalysisH]
nDayAnalyses conn txt n = do
  ctime <- getCurrentTime
  let nBeforeDays = addUTCTime (-(nominalDay * n)) ctime
  analyses <- getRSSAnalysisBySourceAndTime conn (T.unpack txt) nBeforeDays
  return analyses


getNDayAnalyses :: Connection -> Text -> NominalDiffTime -> IO [(Text,Text,Maybe Bool,Maybe Bool,Maybe Bool)]
getNDayAnalyses conn txt n = do
  analyses <- nDayAnalyses conn txt n
  let anList = map (\x -> (T.pack $ bstrHashToB16 $ An._hash x, An._source x, An._corenlp x, An._srl x, An._ner x)) analyses
  return anList

getAnalysesBySrc :: Connection -> T.Text -> Handler [RecentAnalysis]
getAnalysesBySrc conn txt = do
  list <- liftIO $ getNDayAnalyses conn txt 10
  let result = map (\(hsh,src,mb1,mb2,mb3) -> RecentAnalysis hsh src mb1 mb2 mb3) list
  return result

getArticlesBySrc :: Connection -> PathConfig -> T.Text -> T.Text -> T.Text -> Handler (Maybe ItemRSS)
getArticlesBySrc conn cfg src sec hsh = do
  let filepath = (T.intercalate "/" [T.pack (_rssstore cfg),src,sec,"RSSItem",(T.take 2 hsh),hsh])
  ebstr <- liftIO $ try $ B8.readFile (T.unpack filepath)
  case ebstr of
    Left (_e :: IOException) -> return Nothing
    Right bstr -> do
      let mitem = (A.decode . BL.fromStrict) bstr
      return mitem


getRSSArticle :: Connection -> PathConfig -> T.Text -> Handler (Maybe ItemRSS)
getRSSArticle conn cfg hsh = do
  let fps = map (\(x,y,_) -> T.intercalate "/" [T.pack (_rssstore cfg),T.pack x,T.pack y,"RSSItem",(T.take 2 hsh),hsh]) rssAnalysisList
  (ebstrs :: [Either IOException B8.ByteString]) <- liftIO $ mapM (\fp -> try $ B8.readFile (T.unpack fp)) fps
  let bstrs = rights ebstrs
  case bstrs of
    []   -> return Nothing
    x:xs -> let (mitem :: Maybe ItemRSS) = (A.decode . BL.fromStrict) x in case mitem of
      Nothing   -> return Nothing
      Just item -> return (Just item)
