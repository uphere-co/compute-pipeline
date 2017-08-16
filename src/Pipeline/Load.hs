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
                  -> IO [Maybe ([([Text], [Maybe Token], [Maybe Text], [Maybe Text])],
                        [Maybe Sentence], [SentItem], [[Token]], [Maybe PennTree],
                        [Dependency], Maybe [(SentItem, [TagPos (Maybe Text)])])]
loadCoreNLPResult fp = do
  list' <- listDirectory fp
  -- list <- mapM makeAbsolute list'
  let list =  map ((++) (fp ++ "/")) list'
  result <- forM list $ \l -> do
    bstr <- B.readFile l
    let result' = A.decode (BL.fromStrict bstr) :: Maybe ([([Text],[Maybe Token], [Maybe Text], [Maybe Text])],
                                                          [Maybe Sentence], [SentItem], [[Token]],
                                                          [Maybe PennTree], [Dependency],
                                                          Maybe [(SentItem,[TagPos (Maybe Text)])])
    return result'
  return result
