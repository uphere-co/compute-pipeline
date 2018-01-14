{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

module SemanticParserAPI.Compute.Worker where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM         (TMVar)
import           Control.Distributed.Process.Lifted  (SendPort,sendChan)
import           Control.Lens                   ((^.),(^..),_Just,makeLenses)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.HashMap.Strict    as HM
import           Data.IntMap                    (IntMap)
import           Data.Maybe                     (catMaybes)
import           Data.Text                      (Text)
import           Data.Tree                      (Forest)
import qualified Language.Java          as J
import           System.Directory               (getCurrentDirectory,setCurrentDirectory
                                                ,getTemporaryDirectory)
import           System.Process                 (readProcess)
--
import           FrameNet.Query.Frame           (FrameDB,frameDB)
import qualified FrameNet.Type.Definition as F
import           FrameNet.Type.Frame            (frame_definition)
import           NLP.Semantics.Type             (MeaningRoleContent(..),MeaningTree(..)
                                                ,mt_frame,mt_arguments,mt_subordinates
                                                ,mr_content,po_main
                                                )
import           NLP.Syntax.Type.XBar           (lemmaList)
import           NER.Type                       (CompanyInfo)
import           NLP.Type.CoreNLP               (Sentence)
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP            (runParser)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure  (docStructure,mkWikiList)
import           SRL.Analyze.Type               (AnalyzePredata,DocStructure,MeaningGraph
                                                ,ds_mtokenss,ds_sentStructures,ss_tagged
                                                )
import           WikiEL.Type                    (EntityMention)
--
import           CloudHaskell.QueryQueue        (QQVar)
import           CloudHaskell.Util              (LogProcess)
import           SemanticParserAPI.Compute.Type (ComputeQuery(..),ComputeResult(..))


data SRLData = SRLData { _aconfig :: Analyze.Config
                       , _pipeline :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                       , _apredata :: AnalyzePredata
                       , _netagger :: ([Sentence] -> [EntityMention Text])
                       , _forest :: Forest (Either Int Text)
                       , _companyMap :: IntMap CompanyInfo
                       }

makeLenses ''SRLData

allMeaningGraphs :: AnalyzePredata -> IntMap CompanyInfo -> DocStructure -> [MeaningGraph]
allMeaningGraphs apdat cmap dstr =
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map (\sstr -> (sstr,meaningGraph apdat sstr)) sstrs1
  in flip map (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(_mtks,(_i,(sstr,mg))) ->
       let wikilst = mkWikiList cmap sstr
       in tagMG mg wikilst


runSRL :: SRLData -> Text -> IO ([[(Int,Text)]],[MeaningGraph])
runSRL sdat sent = do
  dainput <- runParser (sdat^.pipeline) sent
  dstr <- docStructure (sdat^.apredata) (sdat^.netagger) (sdat^.forest,sdat^.companyMap) dainput
  let sstrs = dstr ^.. ds_sentStructures . traverse . _Just
      tokenss = map (map (\(x,(_,y)) -> (x,y))) $ sstrs ^.. traverse . ss_tagged . lemmaList
      mgs = allMeaningGraphs (sdat^.apredata) (sdat^.companyMap) dstr
  -- dotgraphs <- mapM createDotGraph mgs
  return (tokenss,mgs) --  ,dotgraphs)


queryWorker :: SRLData
            -> QQVar ComputeQuery ComputeResult -- TMVar (HM.HashMap Text ([Int],[Text]))
            -> SendPort ComputeResult
            -> ComputeQuery
            -> LogProcess ()
queryWorker sdat qqvar sc (CQ_Text txt) = do
  -- m <- liftIO $ atomically $ takeTMVar resultref
  -- liftIO $ threadDelay (100*1000000)
  (tokenss,mgs) <- liftIO (runSRL sdat txt)
  -- let tokenss = []
  --     mgs = []
  sendChan sc (CR_TokenMeaningGraph tokenss mgs)


{-
createDotGraph :: MeaningGraph -> IO PNGData
createDotGraph mg = do
  let dotstr = dotMeaningGraph Nothing mg
  cdir <- getCurrentDirectory
  tdir <- getTemporaryDirectory
  setCurrentDirectory tdir
  T.IO.writeFile "test.dot" dotstr
  void (readProcess "/nix/store/hxwdxsg6w79cnj2slkhk3bs8fx6nvdyk-graphviz-2.40.1/bin/dot" ["-Tpng","test.dot","-otest.png"] "")
  bstr <- B.readFile "test.png"
  setCurrentDirectory cdir
  let pngdata = PNGData (T.E.decodeUtf8 ("data:image/png;base64," <> B64.encode bstr))
  return pngdata
-}

{-
allFrames :: MeaningTree -> [Text]
allFrames mt = let frm0 = mt^.mt_frame
               in frm0 : (ys ++ zs)
  where
    xs = mt^..mt_arguments.traverse.mr_content
    ys = concatMap (f . (^.po_main)) xs
    zs = concatMap allFrames (mt^.mt_subordinates)
    --
    f (SubFrame x) = allFrames x
    f (Modifier _ subs) = concatMap allFrames subs
    f _ = []
-}

{-
convertDefRoot :: F.DefRoot -> DefRoot
convertDefRoot (F.DefRoot lst) = DefRoot (map convertCContent lst)
  where
    convertCContent (F.CTEXT txt) = CTEXT txt
    convertCContent (F.CFEN txt)  = CFEN txt
    convertCContent (F.CEX xs)    = CEX (map convertEContent xs)
    convertCContent F.CRET        = CRET ()
    --
    convertEContent (F.ETEXT txt) = ETEXT txt
    convertEContent F.ERET        = ERET ()
    convertEContent (F.EM txt)    = EM txt
    convertEContent (F.EFEX txt)  = EFEX txt
-}

-- deriving instance Show F.DefRoot

{-
mkFrameNetData :: FrameDB -> Text -> (Text,DefRoot)
mkFrameNetData framemap fname = fromMaybe (fname,DefRoot []) $ do
  frm <- HM.lookup fname (framemap^.frameDB)
  let txt = frm^.frame_definition
      defroot0 = F.p_defRoot (T.L.fromStrict txt)
  return (fname,convertDefRoot defroot0)
-}



{-
import           Control.Concurrent.STM
import           Control.Distributed.Process.Lifted
import           Control.Monad
import           Control.Monad.IO.Class                    (MonadIO(liftIO))
import           Control.Monad.Trans.Maybe                 (MaybeT(MaybeT,runMaybeT))
import           Data.Aeson
import qualified Data.ByteString.Char8               as B
import qualified Data.ByteString.Lazy.Char8          as BL
import           Data.ByteString.Unsafe                    (unsafePackCString)
import qualified Data.HashMap.Strict                 as HM
import           Data.Text                                 (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as TE
import           Foreign.C.String
import           System.IO
--
import           CloudHaskell.Server
import           Query.Binding
import           QueryServer.Type
import           CoreNLP

registerText :: (MonadIO m) => String -> EngineWrapper -> Text -> MaybeT m RegisteredSentences
registerText corenlp_server engine txt = do
  guard ((not . T.null) txt)
  bstr_nlp0 <- (liftIO . runCoreNLP corenlp_server . TE.encodeUtf8) txt
  guard ((not . B.null) bstr_nlp0)
  bstr0 <- liftIO $
    B.useAsCString bstr_nlp0 $ \cstr_nlp0 -> do
      withCString "did_you_mean" $ \did_you_mean -> do
        B.hPutStrLn stderr "step1"
        bstr_nlp1 <- json_tparse cstr_nlp0 >>= preprocess_query engine >>= \j -> find j did_you_mean >>= B.packCString
        B.hPutStrLn stderr "step2"
        bstr_nlp2 <- runCoreNLP corenlp_server bstr_nlp1
        B.useAsCString bstr_nlp1 $ \cstr_nlp1 -> do
          -- bstr_nlp1 <- packCString cstr_nlp1
          B.useAsCString bstr_nlp2 $ \cstr_nlp2 ->
            json_tparse cstr_nlp2 >>= register_documents engine cstr_nlp1 >>= serialize >>= B.packCString
  liftIO $ putStrLn "inside registerText"
  liftIO $ print bstr0
  (MaybeT . return . decodeStrict') bstr0


queryRegisteredSentences :: EngineWrapper -> RegisteredSentences -> IO BL.ByteString
queryRegisteredSentences engine r = do
  -- need to be configured.
  let bstr = BL.toStrict $ encode (r { rs_max_clip_len = Just 200 })
  bstr' <- B.useAsCString bstr $
     json_tparse >=> query engine >=> serialize >=> unsafePackCString
  return (BL.fromStrict bstr')

querySuggestion :: EngineWrapper -> [Text] -> IO BL.ByteString
querySuggestion engine ideas = do
  let bstr = BL.toStrict $ encode (object [ "ideas" .= ideas ])
  bstr' <- B.useAsCString bstr $
     json_tparse >=> suggest engine >=> serialize >=> unsafePackCString
  return (BL.fromStrict bstr')

type ResultBstr = BL.ByteString

failed :: BL.ByteString
failed = encode Null


-}
