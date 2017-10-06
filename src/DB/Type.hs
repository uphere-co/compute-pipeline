module DB.Type where

class UpHereDB a where
  upload  :: a -> IO ()
  upload2 :: a -> IO ()
  update  :: a -> IO ()

data ArticleDB = ArticleDB { }
data AnalysisDB = AnalysisDB { }
data ArticleErrorDB = ArticleErrorDB { }

class Article a where
  toArticle :: a -> ArticleDB

class Analysis a where
  toAnalysis :: a -> AnalysisDB

class ArticleError a where
  toArticleError :: a -> ArticleErrorDB
