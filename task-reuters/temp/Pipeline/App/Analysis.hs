{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.Analysis where

import           Control.Concurrent                (threadDelay)
import           Control.Exception                 (SomeException,handle)
import           Control.Lens                      ((^.))
import           Control.Monad                     (forever,forM_,void)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                     (traverse_)
import           Data.IntMap                       (IntMap)
import           Data.List.Split                   (chunksOf)
import           Data.Maybe
import           Data.Range
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock                   (UTCTime,getCurrentTime)
import           Data.Tree                         (Forest)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple        (Connection)
import           System.Directory                       (getCurrentDirectory,setCurrentDirectory)
import           System.FilePath                        ((</>),(<.>),addExtension,takeBaseName)
import           System.Process                         (readProcess)
--
import           Data.Graph.Algorithm.Basic        (maxConnectedNodes,numberOfIsland)
import           DB.Schema.RSS
import           DB.Schema.RSS.SRL
import           Lexicon.Data                      (loadLexDataConfig)
import           MWE.Util                               (mkTextFromToken)
import           NER.Type                          (CompanyInfo(..))
import           NLP.Shared.Type                   (PathConfig,EventClass(..)
                                                   ,dbstring,lexconfigpath,mgdotfigstore)
import           NLP.Type.CoreNLP                  (Sentence,Token(..))
import           NLP.Type.TagPos                   (leftTagPos)
import           SRL.Analyze                       (loadConfig)
import           SRL.Analyze.ARB                   (mkARB)
import           SRL.Analyze.Format                (dotMeaningGraph)
import           SRL.Analyze.Match.MeaningGraph    (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure     (docStructure,mkWikiList)
import           SRL.Analyze.Type                  (MeaningGraph,DocAnalysisInput
                                                   ,AnalyzePredata,SentStructure
                                                   ,analyze_rolemap
                                                   ,ds_mergedtags
                                                   ,ds_mtokenss
                                                   ,ds_sentStructures
                                                   )
import           SRL.Statistics                    (getGraphFromMG)
import           Text.Format.Dot                   (mkLabelText)
import           WikiEL.Type                       (EntityMention)
--
import           Pipeline.Operation.Concurrent     (forkChild,refreshChildren,waitForChildren)
import           Pipeline.Operation.DB             (closeConnection,getConnection)
import           Pipeline.Source.RSS.Article       (listNewDocAnalysisInputs)
import           Pipeline.Type                     (SourceTimeConstraint)
import           Pipeline.Util                     (saveHashNameBSFileInPrefixSubDirs,splitPrefixSubDirs)


-- | this should be dynamically determined.
coreN :: Int
coreN = 15

isSRLFiltered :: SentStructure -> MeaningGraph -> Bool
isSRLFiltered _sstr mg =
  let -- a = numberOfPredicate sstr
      -- b = numberOfMGVerbPredicate mg
      mgraph = getGraphFromMG mg
      (c,d) = case mgraph of
                Nothing -> (-1,-1)
                Just gr -> (maxConnectedNodes gr, numberOfIsland gr)
  in ({- (a == b) &&  -}(c >=4) && (d < 3))



listPoliticalEvent,listCentralBanks,listAccidentAndTension,listOilDemand :: [Text]
listTanker,listStockMov,listOilTrade,listShaleOil,listOPEC,listMarketData :: [Text]

--listPoliticalEvent = ["Brexit","lust belt","sanction","doctrine","deplomatic","deplomat","embassy","missle","bureaucracy","dictator","dictatorship","communism","capitalism","monarchy","nationalism","sovereign"]
listPoliticalEvent = ["presidential election","republican","GOP","Grand Old Party","Democratic","White House","Blue House","congress","senate","concurrent resolution","organization"]
listCentralBanks = ["FRB","Federal Reserve","ECB","European Central Banks","BOJ","Bank of Japan","BOE","bank of England","GDP"]
listAccidentAndTension = ["North Korea","Kim Jung Eun"]



listOilDemand = ["naphtha"," gasoil"," gasoline"," LNG"," LPG","Fuel Oil"," brent","natural gas","crude oil" , " oil rig", " rig"]
listTanker = ["tanker spot freight rates","VLCC","suezmax","aframax","vessel availability","clean tanker","dirty tanker"
             ,"spot fixture","OPEC sailings"," oil arrivals","tanker market","tanker freight rate","MR tanker","medium-range tanker","VGO"]
listStockMov = [" oil stock","gas stock","LNG stock","LPG stock","naphtha stock","gasoil stock","gasoline stock"," oil inventory"
               ,"gas inventory","LNG inventory","LPG inventory","crude stock","brent stock"]
listOilTrade = [" oil import"," oil export","crude supplier","refinery throughput","lukoil system"]
listShaleOil = ["shale oil","shale gas"]
listOPEC       = [ "OPEC" ]
listMarketData = ["refinery margin","refinery ultilization","refinery ultilisation","crack spreads"," oil crack", " oil rig", "OPEC"
                 ,"reference bascket"," oil option"," oil future","light crude","sour crude","sweet crude"]

evtClass :: [Text] -> [EventClass]
evtClass txts =
  let lowtxt = T.intercalate " " $ map T.toLower txts

      isOilDemand  = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listOilDemand)  then Just (EventClass "Commodities" "Energy" (Just "OilDemand"))     else Nothing
      isTanker     = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listTanker)     then Just (EventClass "Commodities" "Energy" (Just "Tanker"))        else Nothing
      isStockMov   = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listStockMov)   then Just (EventClass "Commodities" "Energy" (Just "StockMovement")) else Nothing
      isOilTrade   = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listOilTrade)   then Just (EventClass "Commodities" "Energy" (Just "OilTrade"))      else Nothing
      isShaleOil   = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listShaleOil)   then Just (EventClass "Commodities" "Energy" (Just "ShaleOil"))      else Nothing
      isMarketData = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listMarketData) then Just (EventClass "Commodities" "Energy" (Just "MarketData"))    else Nothing
      isOPEC       = if (any (\x -> T.toLower x `T.isInfixOf` lowtxt) listOPEC)       then Just (EventClass "Commodities" "Energy" (Just "OPEC"))          else Nothing
  in catMaybes [isOilDemand,isTanker,isStockMov,isOilTrade,isShaleOil,isMarketData,isOPEC]


mkMGs :: Connection
      -> AnalyzePredata
      -> ([Sentence] -> [EntityMention Text])
      -> (Forest (Either Int Text),IntMap CompanyInfo)
      -> PathConfig
      -> B.ByteString
      -> UTCTime
      -> DocAnalysisInput
      -> IO ()
mkMGs conn apredata netagger (forest,companyMap) _cfg hsh time input = do
  -- let filename = takeFileName fp
  dstr <- docStructure apredata netagger (forest,companyMap) input
  let sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      _netags = leftTagPos (dstr^.ds_mergedtags)
      texttoken = map (_token_text) ((catMaybes . concat) mtokss)
      _evtcls = evtClass texttoken
      mgs = map (meaningGraph apredata) sstrs
      arbs = map (mkARB (apredata^.analyze_rolemap)) mgs -- map (mkMeaningTree (apredata^.analyze_rolemap)) mgs
      _wikilsts = map (mkWikiList companyMap) sstrs
      _isNonFilter = True -- False
  let result = (mgs,arbs)
  let rtxt = (TE.decodeUtf8 . BL.toStrict . A.encode) result
  runBeamPostgres conn $
    let srl :: AnalysisSRL
        srl = AnalysisSRL hsh (Just rtxt) time
    -- only support insert
    in runInsert $
         insert (_SRLs rssDB) $
           insertValues [srl]
{-
    as' <-
      runSelectReturningList $
        select $
          filter_ (\s -> s^.srlHash ==. val_ hsh) (all_ (_SRLs rssDB))
    case as' of
      []  ->
      _as -> runUpdate $
               update (_SRLs rssDB)
                      (\s -> [ s^.srlResult  <-. val_ (Just rtxt)
                             , s^.srlCreated <-. val_ time
                             ])
                      (\s -> s^.srlHash ==. val_ hsh)
  -}
  --  print A.encode (mgs,arbs)
  {-
  putStrLn $ "Analyzing " ++ filename
  saveMGs (cfg ^. mgstore) filename mgs -- Temporary solution
  forM_ (zip6 ([1..] :: [Int]) sstrs mtokss mgs arbs wikilsts) $ \(i,sstr,mtks,mg,arb,wikilst) -> do
    when (isSRLFiltered sstr mg || isNonFilter) $ do
      putStrLn $ filename ++ " is filtered in!"
      putStrLn $ filename ++ ": saving MGS"
      saveMG (cfg ^. mgstore) filename i mg
      putStrLn $ filename ++ ": saving ARB"
      saveARB (cfg ^. arbstore) filename i (tm,(arb,netags,evtcls))
      putStrLn $ filename ++ ": saving DOT"
      genMGFigs cfg filename i sstr mtks mg wikilst
  -}

genMGFigs :: PathConfig -> FilePath -> Int -> SentStructure -> [Maybe Token] -> MeaningGraph -> [(Range, Text)] -> IO ()
genMGFigs cfg filename i _sstr mtks mg wikilst = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing -> return ()
    Just _graph -> do
      let mg' = tagMG mg wikilst
      mkMGDotFigs (cfg ^. mgdotfigstore) i filename mtks mg'

runAnalysisByChunks :: Connection
                    -> ([Sentence] -> [EntityMention Text])
                    -> (Forest (Either Int Text),IntMap CompanyInfo)
                    -> AnalyzePredata
                    -> PathConfig
                    -> [(B.ByteString,Maybe DocAnalysisInput)]
                    -> IO ()
runAnalysisByChunks conn netagger (forest,companyMap) apredata cfg loaded = do
  flip mapM_ loaded $ \(hsh,minput) -> do
    handle (\(e :: SomeException) -> print e) $ do
      tm <- getCurrentTime
      {- updateRSSAnalysisStatus
        conn
        (b16ToBstrHash (T.pack (takeBaseName fp)))
        (Nothing,Just True,Nothing) -}
      traverse_ (mkMGs conn apredata netagger (forest,companyMap) cfg hsh tm) minput

-- | This does SRL and generates meaning graphs.
--
runSRL :: Connection
       -> AnalyzePredata
       -> ([Sentence] -> [EntityMention T.Text])
       -> (Forest (Either Int Text), IntMap CompanyInfo)
       -> PathConfig
       -> SourceTimeConstraint
       -> IO ()
runSRL conn apredata netagger (forest,companyMap) cfg (msrc,tc) = do
  loaded <- listNewDocAnalysisInputs cfg (msrc,tc)
  let n = length loaded `div` coreN
  forM_ (chunksOf n loaded) $ \ls ->
    forkChild (runAnalysisByChunks conn netagger (forest,companyMap) apredata cfg ls)

  waitForChildren
  refreshChildren

-- -----------------------------------------------------------------

mkMGDotFigs :: (Show a) => FilePath -> a -> FilePath -> [Maybe Token] -> MeaningGraph -> IO ()
mkMGDotFigs savedir i filename mtks mg = do
  let title = mkTextFromToken mtks
      dottxt = dotMeaningGraph (Just (mkLabelText title)) mg
      filepath = (savedir </> filename) ++ "_" ++ (show i) ++ ".dot"

  saveHashNameBSFileInPrefixSubDirs filepath (TE.encodeUtf8 dottxt)
  let fname = takeBaseName filepath
  let (_hsh,storepath,prefix) = splitPrefixSubDirs filepath
  dir <- getCurrentDirectory
  setCurrentDirectory (storepath </> prefix)
  void $ readProcess "dot" ["-Tpng",fname <.> "dot","-o",fname <.>"png"] ""
  setCurrentDirectory dir


saveJSON :: A.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveJSON savedir filename jsondata =
  saveHashNameBSFileInPrefixSubDirs
    (savedir </> filename)
    (BL.toStrict (A.encode jsondata))

saveMGs :: A.ToJSON a => FilePath -> FilePath -> a -> IO ()
saveMGs savedir filename mgs =
  saveJSON savedir (addExtension filename "mgs") mgs

saveMG :: A.ToJSON a => FilePath -> FilePath -> Int -> a -> IO ()
saveMG savedir filename i mg =
  saveJSON savedir (addExtension (filename ++ "_" ++ (show i)) "mgs") mg

saveARB :: A.ToJSON a => FilePath -> FilePath -> Int -> a -> IO ()
saveARB savedir filename i arb =
  saveJSON savedir (addExtension (filename ++ "_" ++ (show i)) "arb") arb

saveWikiEL :: A.ToJSON a => FilePath -> a -> IO ()
saveWikiEL fp wikiel = B.writeFile (fp ++ ".wiki") (BL.toStrict $ A.encode wikiel)

-- -----------------------------------------------------------------

type Source = String
type Section = String
type RSSLink = String


constraint :: SourceTimeConstraint
constraint = (Just "reuters/Archive",Nothing)



runDaemon :: PathConfig -> IO ()
runDaemon cfg = do
  conn <- getConnection (cfg ^. dbstring)
  cfgG <- (\ec -> case ec of {Left err -> error err;Right c -> return c;}) =<< loadLexDataConfig (cfg ^. lexconfigpath)
  (apredata,netagger,forest,companyMap) <- loadConfig (False,False) cfgG
  forever $ do
    runSRL conn apredata netagger (forest,companyMap) cfg constraint
    putStrLn "Waiting next run..."
    let sec = 1000000 in threadDelay (60*sec)
  closeConnection conn



