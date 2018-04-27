module Pipeline.Source.RSS.Analysis where

import           Control.Lens                      ((^.),to)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BL8  
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock                   (UTCTime)
import           Database.Beam
import           Database.Beam.Postgres (runBeamPostgresDebug)
import           System.FilePath                   ((</>),takeBaseName)
--
import           DB.Operation.RSS.Analysis         (queryAnalysis,bySource)
import           DB.Schema.RSS
import           DB.Schema.RSS.Analysis
import           NLP.Shared.Type                   (PathConfig,dbstring)
--
import           Pipeline.Operation.DB             (getConnection)
import           Pipeline.Type


getAllRSSAnalysisFilePath :: PathConfig -> IO [(FilePath,UTCTime)]
getAllRSSAnalysisFilePath cfg = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runBeamPostgresDebug putStrLn conn $
          runSelectReturningList $
            select $
              all_ (_rssAnalyses rssDB)
  pure $ map mkPair as


getRSSAnalysisFilePathBySource :: PathConfig -> Text -> IO [(FilePath,UTCTime)]
getRSSAnalysisFilePathBySource cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runBeamPostgresDebug putStrLn conn $
          runSelectReturningList $
            select $
              queryAnalysis (bySource src)
  pure $ map mkPair as


getNewItemsForSRL :: PathConfig -> Text -> IO [(FilePath,UTCTime)]
getNewItemsForSRL cfg src = do
  conn <- getConnection (cfg ^. dbstring)
  as <- runBeamPostgresDebug putStrLn conn $
          runSelectReturningList $
            select $
              queryAnalysis $ \a ->     bySource src a
                                    &&. (a^.rssAnalysisCoreNLP ==. val_ (Just True))
                                    &&. (    (a^.rssAnalysisSRL ==. val_ Nothing )
                                         ||. (a^.rssAnalysisSRL ==. val_ (Just False)))
  pure $ map mkPair as



mkPair :: RSSAnalysis -> (FilePath,UTCTime)
mkPair a = let hsh = ranHshB16 a in (take 2 (T.unpack hsh) </> T.unpack hsh,a^.rssAnalysisCreated)



ranHshB16 :: RSSAnalysis -> Text
ranHshB16 x = x^.rssAnalysisHash . to (TE.decodeUtf8 . B16.encode)
