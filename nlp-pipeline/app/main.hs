module Main where

--
import Pipeline.Util


main :: IO ()
main = do
  return ()
  {-
  opt <- execParser progOption
  pgconn <- PGS.connectPostgreSQL (B.pack ("dbname=" ++ dbname opt))
  forest <- prepareForest (entityFile opt)
  cnts <- getDirectoryContents (dir opt)
  let cnts' = map (dir opt </>) $ sort $ filter (\p -> takeExtensions p == ".maintext") cnts
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    let pcfg = PPConfig True True True True True
    pp <- prepare pcfg
    mapM_ (process pgconn pp forest) cnts'
  PGS.close pgconn
  -}

