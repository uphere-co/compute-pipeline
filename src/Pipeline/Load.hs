
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
import 	       	 NLP.Type.CoreNLP
import           NLP.Type.PennTreebankII
import           SRL.Analyze.Type
import           SRL.Analyze.Util
import           WikiEL.EntityLinking           (EntityMention)

loadCoreNLPResult :: [FilePath]
                  -> IO [(FilePath, Maybe DocAnalysisInput)]
loadCoreNLPResult fps = do
  forM fps $ \fp -> do
    bstr <- B.readFile fp
    return $ (fp,A.decode (BL.fromStrict bstr))

loadWikiELResult :: [FilePath] -> IO [(FilePath,Maybe [EntityMention Text])]
loadWikiELResult fps = do
  forM fps $ \fp -> do
    bstr <- B.readFile fp
    return $ (fp,A.decode (BL.fromStrict bstr))

getFileListRecursively fp = do
  list' <- readDirectoryWith return fp
  let filelist = sort . toList $ dirTree list'
  return filelist

