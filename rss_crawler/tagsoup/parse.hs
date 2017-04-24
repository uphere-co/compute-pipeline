{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Text (Text)
import qualified Data.Vector                as V
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  str <- BL.readFile (args !! 0)
  let ev :: Either String Value = eitherDecode str
  case ev of
    Left err -> print err
    Right v -> do
      case fmap (V.take 10000) (v ^? _Array) of
        Nothing -> return ()
        Just vs -> do
          let lst = V.toList vs
              lst' = do
                Object x <- lst
                (y,Array ys) <- HM.toList x
                return (y, length ys)
          -- print (HS.fromList lst')
          let multiples = filter (\(_,n) -> n > 1) lst'
          mapM_ print multiples
          print $ HS.fromList (map fst multiples)
