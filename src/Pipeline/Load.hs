module Pipeline.Load where


import           Control.Monad                  (forM)
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Foldable                  (toList)
import           Data.List                      (sort)
import           Data.Text                      (Text)
import           System.Directory.Tree
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           OntoNotes.App.Util

loadCoreNLPResult :: FilePath
                  -> IO [Maybe ( [Sentence]
                               , [Maybe SentenceIndex]
                               , [SentItem CharIdx]
                               , [[Token]]
                               , [Maybe PennTree]
                               , [Dependency]
                               , Maybe [TagPos TokIdx (Maybe Text)]
                               )
                        ]
loadCoreNLPResult fp = do
  list' <- readDirectoryWith return fp
  let list = sort . toList $ dirTree list'
  forM list $ \l -> do
    bstr <- B.readFile l
    return $ A.decode (BL.fromStrict bstr)
