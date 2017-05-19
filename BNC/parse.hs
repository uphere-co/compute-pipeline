{-# LANGUAGE OverloadedStrings #-}
 
import           Control.Lens                  ((^?),(^.),(^..), ix,only )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as TE
import           Data.Text.Encoding.Error      (lenientDecode)
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TLIO
import           Text.Taggy.Lens

main :: IO ()
main = do
  let fp = "codes.html"
  putStrLn fp
  bstr <- B.readFile fp
  let txt = (TL.fromStrict . TE.decodeUtf8With lenientDecode) bstr 
  let b = head (txt ^.. html . allNamed (only "body"))
      d = do
        x <- b ^.. allNamed (only "tr")
        -- let ys = x ^.. allNamed (only "td") . element . contents
        return x
  -- print (length d)
  -- mapM_ print d
  let d' = (drop 428 . take 969) d
      d'' = do
        y <- d'
        let xs = map (^.contents) (y ^.. allNamed (only "td"))
        return xs
  mapM_ print d''
