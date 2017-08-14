module Pipeline.Load where


import           Control.Monad                  (forM_)
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Text                      (Text)
import           System.Directory               (listDirectory,makeAbsolute)
import           System.FilePath                ((</>))
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           OntoNotes.App.Util

loadCoreNLPResult fp = do
  list' <- listDirectory fp
  -- list <- mapM makeAbsolute list'
  let list =  map ((++) (fp ++ "/")) list'
  forM_ list $ \l -> do
    bstr <- BL.readFile l
    let result = A.decode bstr :: Maybe ([[Text]], [Maybe Sentence], [SentItem], [[Token]],
                                       [Maybe PennTree], [Dependency],
                                       Maybe [[(CharIdx, CharIdx, Maybe Text)]])
    print result
