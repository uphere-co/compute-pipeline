{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import           Data.ByteString.Handle
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Unsafe     as BU
import           Data.Compact
import           Data.Compact.Serialize
import           Data.Int
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import           System.IO
import           System.Directory
import           System.Environment               (getArgs)
import           System.Mem

import           System.IO.MMap
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           Foreign.ForeignPtr
import           Foreign.Ptr (castPtr)

import           Data.Vector                      (Vector,(!))
import qualified Data.Vector                as V
import qualified Data.Vector.Storable       as VS


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

giga = 1000000000

main4 = do
    let val = VS.generate giga fromIntegral :: VS.Vector Int32
    -- c <- compact val -- compactWithSharing val
    let fp = "vector.bin"
    
    mmapWithFilePtr fp ReadWriteEx (Just (0,sizeOf (undefined :: Int32) * giga)) $ \(ptr,size) -> do
      VS.unsafeWith val $ \ptr' -> do
        copyBytes (castPtr ptr) ptr' size 
    --   flip VS.foriM val $ \v -> 
    --     poke (castPtr ptr + ) val 
-- ==  tmp "compact.bin"
    -- print fp
    -- hPutCompact h c
    -- hClose h
    -- performMajorGC

main5 = do
  {- (ptr,rawsize,offset,size) <- mmapFilePtr "compact.bin" ReadOnly Nothing
  print (rawsize,offset,size)
  x :: Int <- peekByteOff ptr 10000000 
  print x
  -}
  bstr <- mmapFileByteString "vector.bin" Nothing
  -- print (B.length bstr)
  -- print (B.elemIndex '9' bstr)
  BU.unsafeUseAsCString bstr $ \cstr -> do
    fptr <- newForeignPtr_ (castPtr cstr)
    let vs = VS.unsafeFromForeignPtr0 fptr giga :: VS.Vector Int32
    print (vs VS.! 292901201) 
    
  
  {- 
  h <- readHandle True (BL.fromStrict bstr)
  
  r <- hUnsafeGetCompact @(Vector Value) h 
  c' <- case r of
          Left err -> fail err
          Right x -> return x
  print "hello"
  print ((getCompact c') ! 1298930) 
  -}
  
main = do
  r <- getArgs
  case r !! 0 of
    "0" -> main0
    "1" -> main1
    "2" -> main2
    "3" -> main3
    "4" -> main4
    "5" -> main5
