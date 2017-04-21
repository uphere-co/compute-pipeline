{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BL
import           Data.Compact
import           Data.Compact.Serialize
import           Data.Text                   (Text)
import qualified Data.Text              as T
import           System.IO
import           System.Directory
import           System.Environment (getArgs)
import           System.Mem
import           Data.Vector      (Vector,(!))
import qualified Data.Vector as V

testset :: Value
testset = object ["test" .= ("TEXT" :: Text)]


main0 = do 
  let bstr = encode (replicate 10000000 testset)
  BL.writeFile "test.json" bstr 
  
main1 = do
  lbstr <- BL.readFile "test.json"
  let mvs = decode lbstr :: Maybe [Value]
  case mvs of
    Nothing -> error "error in decode"
    Just vs -> print (length vs)
  
main2 = do
    -- let val = ("hello", 1, 42, 42, Just 42) :: (String, Int, Int, Integer, Maybe Int)
    -- let val = [1..100000] :: [Int]
    let val = V.replicate 10000000 testset
    c <- compact val -- compactWithSharing val
    let fp = "compact.bin"
    h <- openFile fp WriteMode -- ==  tmp "compact.bin"
    -- print fp
    hPutCompact h c
    hClose h
    performMajorGC




main3 = do
    let fp = "compact.bin"
    r <- unsafeReadCompact @(Vector Value) fp
    -- r <- unsafeReadCompact @[Int] fp
    -- r <- unsafeReadCompact @(String, Int, Int, Integer, Maybe Int) fp
    c' <- case r of
            Left err -> fail err
            Right x -> return x
    print "hello"
    -- print (length (getCompact c'))
    -- threadDelay 10000000
    --- print (length (getCompact c'))
    print ((getCompact c') ! 1298930) 
    
    -- removeFile fp
    -- print (getCompact c')
    -- when (val /= getCompact c') $ fail "did not match"
    -- putStrLn "OK"

main = do
  r <- getArgs
  case r !! 0 of
    "0" -> main0
    "1" -> main1
    "2" -> main2
    "3" -> main3
    
