{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline.App.AnalysisRunner where

import           Control.Exception                      (SomeException,handle)
import           Control.Lens
import           Control.Monad                          (forM_,when)
import           Data.Char                              (isSpace)
import           Data.List                              (zip6)
import           Data.Maybe
import           Data.Text                              (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple             (Connection)
import           System.FilePath                        ((</>),takeFileName,takeBaseName)
--
import           Data.Range                             (Range)
import           Data.Time.Clock                        (UTCTime,getCurrentTime)
import           Data.Graph.Algorithm.Basic             (maxConnectedNodes,numberOfIsland)
import           DB.Operation                           (updateRSSAnalysisStatus)
import           DB.Util                                (b16ToBstrHash)
import           Lexicon.Data                           (loadLexDataConfig)
import           MWE.Util                               (mkTextFromToken)
import           NewsAPI.DB
import           NLP.Shared.Type                        (PathConfig
                                                        ,arbstore,corenlpstore,lexconfigpath
                                                        ,mgstore,mgdotfigstore)
import           NLP.Type.CoreNLP
import           NLP.Type.NamedEntity                   (NamedEntityClass)
import           NLP.Type.TagPos                        (leftTagPos)
import           SRL.Analyze
import           SRL.Analyze.ARB                        (mkARB)
import           SRL.Analyze.Match                      (meaningGraph)
import           SRL.Analyze.Match.MeaningGraph         (changeMGText,tagMG)
import           SRL.Analyze.SentenceStructure          (docStructure,mkWikiList)
import           SRL.Analyze.Type
-- import qualified SRL.Analyze.WikiEL         as SRLWiki
import           SRL.Statistics
import           WikiEL.Type                            (EntityMention)
--
import           Pipeline.Load
import           Pipeline.Run
import           Pipeline.Type
import           Pipeline.Util



isSRLFiltered sstr mg =
 let a = numberOfPredicate sstr
     b = numberOfMGVerbPredicate mg
     mgraph = getGraphFromMG mg
     (c,d) = case mgraph of
               Nothing -> (-1,-1)
               Just gr -> (maxConnectedNodes gr, numberOfIsland gr)
 in ({- (a == b) &&  -}(c >=4) && (d < 3))



--listPoliticalEvent = ["Brexit","lust belt","sanction","doctrine","deplomatic","deplomat","embassy","missle","bureaucracy","dictator","dictatorship","communism","capitalism","monarchy","nationalism","sovereign"]
listPoliticalEvent = ["presidential election","republican","GOP","Grand Old Party","Democratic","White House","Blue House","congress","senate","concurrent resolution","organization"]
listCentralBanks = ["FRB","Federal Reserve","ECB","European Central Banks","BOJ","Bank of Japan","BOE","bank of England","GDP"]
listAccidentAndTension = ["North Korea","Kim Jung Eun"]



listOilDemand = [" naphtha"," gasoil"," gasoline"," LNG"," LPG","Fuel Oil"," brent","natural gas","crude oil" , "oil rig"]
listTanker = ["tanker spot freight rates","VLCC","suezmax","aframax","vessel availability","clean tanker","dirty tanker"
             ,"spot fixture","OPEC sailings","oil arrivals","tanker market","tanker freight rate","MR tanker","medium-range tanker","VGO"]
listStockMov = ["oil stock","gas stock","LNG stock","LPG stock","naphtha stock","gasoil stock","gasoline stock","oil inventory"
               ,"gas inventory","LNG inventory","LPG inventory","crude stock","brent stock"]
listOilTrade = ["oil import","oil export","crude supplier","refinery throughput","lukoil system"]
listShaleOil = ["shale oil","shale gas"]
listOPEC       = [ "OPEC" ]
listMarketData = ["refinery margin","refinery ultilization","refinery ultilisation","crack spreads","oil crack", "oil rig", "OPEC"
                 ,"reference bascket","oil option","oil future","light crude","sour crude","sweet crude"]

evtClass :: [Text] -> Text
evtClass txts =
  let lowtxt = T.intercalate " " $ map T.toLower txts
      isOilDemand = if (any (flip T.isInfixOf lowtxt) listOilDemand) then Just "OilDemand" else Nothing
      isTanker = if (any (flip T.isInfixOf lowtxt) listTanker) then Just "Tanker" else Nothing
      isStockMov = if (any (flip T.isInfixOf lowtxt) listStockMov) then Just "StockMovement" else Nothing
      isOilTrade = if (any (flip T.isInfixOf lowtxt) listOilTrade) then Just "OilTrade" else Nothing
      isShaleOil = if (any (flip T.isInfixOf lowtxt) listShaleOil) then Just "ShaleOil" else Nothing
      isMarketData = if (any (flip T.isInfixOf lowtxt) listMarketData) then Just "MarketData" else Nothing
      isOPEC = if (any (flip T.isInfixOf lowtxt) listOPEC) then Just "OPEC" else Nothing
  in T.intercalate "/" $ catMaybes [isOilDemand,isTanker,isStockMov,isOilTrade,isShaleOil,isMarketData,isOPEC]

mkMGs :: Connection
      -> AnalyzePredata
      -> ([Sentence] -> [EntityMention Text])
      -> PathConfig
      -> FilePath
      -> UTCTime
      -> DocAnalysisInput
      -> IO ()
mkMGs conn apredata netagger cfg fp tm article = do
  let filename = takeFileName fp
      dstr = docStructure apredata netagger article
      sstrs = catMaybes (dstr ^. ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      netags = leftTagPos (dstr^.ds_mergedtags)
      texttoken = map (_token_text) ((catMaybes . concat) mtokss)
      evtcls = evtClass texttoken
      mgs = map meaningGraph sstrs
      arbs = map (mkARB (apredata^.analyze_rolemap)) mgs
      wikilsts = map mkWikiList sstrs
      isNonFilter = False
  print evtcls
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

genMGFigs :: PathConfig -> FilePath -> Int -> SentStructure -> [Maybe Token] -> MeaningGraph -> [(Range, Text)] -> IO ()
genMGFigs cfg filename i sstr mtks mg wikilst = do
  let mgraph = getGraphFromMG mg
  case mgraph of
    Nothing -> return ()
    Just graph -> do
      let mg' = tagMG mg wikilst
      mkMGDotFigs (cfg ^. mgdotfigstore) i filename mtks mg'

runAnalysisByChunks :: Connection -> ([Sentence] -> [EntityMention Text])
                    -> AnalyzePredata -> PathConfig -> [(FilePath,UTCTime,DocAnalysisInput)] -> IO ()
runAnalysisByChunks conn netagger apredata cfg loaded = do
  flip mapM_ loaded $ \(fp,tm,artl) -> do
    handle (\(e :: SomeException) -> print e) $ do
      updateRSSAnalysisStatus conn (b16ToBstrHash (takeBaseName fp)) (Nothing,Just True,Nothing)
      mkMGs conn apredata netagger cfg fp tm artl
