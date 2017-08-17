module Pipeline.Load where


import           Control.Monad                  (forM)
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Text                      (Text)
import           System.Directory               (listDirectory)
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           OntoNotes.App.Util

loadCoreNLPResult :: FilePath
                  -> IO [Maybe ([Sentence], [Maybe SentenceIndex], [SentItem], [[Token]], [Maybe PennTree],
                        [Dependency], Maybe [(SentItem, [TagPos (Maybe Text)])])]
loadCoreNLPResult fp = do
  list <- map ((++) (fp ++ "/")) <$> listDirectory fp
  forM list $ \l -> do
    bstr <- B.readFile l
    return $ A.decode (BL.fromStrict bstr)
