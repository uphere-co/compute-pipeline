{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}



import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Unsafe     as BU
import           Data.Compact
import           Data.Compact.Serialize
import           Data.Int
import           Data.Text                        (Text)
import           Data.Vector                      (Vector,(!))
import qualified Data.Vector                as V
import qualified Data.Vector.Storable       as VS
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           Foreign.ForeignPtr
import           Foreign.Ptr (castPtr)
import           System.IO
import           System.Environment               (getArgs)
import           System.Mem
import           System.IO.MMap
import           System.Random                    (randomRIO)
--

gen ::  Int -> Value
gen n
  | n `mod` 3 == 0 = object [ "test" .= ("SET0" :: Text) ]
  | n `mod` 3 == 1 = object [ "test" .= ("SET1" :: Text) ]
  | otherwise      = object [ "test" .= ("SET2" :: Text) ]

testvector :: Vector Value
testvector = V.generate (giga `div` 100) gen

main0 :: IO ()
main0 = do 
  let bstr = encode testvector
  BL.writeFile "test.json" bstr 

main1 :: IO ()
main1 = do
  bstr <- BL.readFile "test.json"
  let mvs = decode bstr :: Maybe (Vector Value)
  case mvs of
    Nothing -> error "fail parsing"
    Just vs -> do
      n <- randomRIO (0, giga `div` 100)
      print (vs ! n)

main2 :: IO ()
main2 = do
    c <- compact testvector
    -- c <- compactWithSharing testvector
    let fp = "compact.bin"
    h <- openFile fp WriteMode
    hPutCompact h c
    hClose h
    performMajorGC

main3 :: IO ()
main3 = do
    let fp = "compact.bin"
    r <- unsafeReadCompact @(Vector Value) fp
    c' <- case r of
            Left err -> fail err
            Right x -> return x
    replicateM_ 10000 $ do
      n <- randomRIO (0,giga `div` 100)
      print ((getCompact c') ! n) 



giga :: Int
giga = 1000000000

main4 :: IO ()
main4 = do
    let val = VS.generate giga fromIntegral :: VS.Vector Int32
    let fp = "vector.bin"
    mmapWithFilePtr fp ReadWriteEx (Just (0,sizeOf (undefined :: Int32) * giga)) $ \(ptr,size) -> do
      VS.unsafeWith val $ \ptr' -> do
        copyBytes (castPtr ptr) ptr' size 

main5 :: IO ()
main5 = do
  replicateM_ 100 $ do
    bstr <- mmapFileByteString "vector.bin" Nothing
    BU.unsafeUseAsCString bstr $ \cstr -> do
      fptr <- newForeignPtr_ (castPtr cstr)
      let vs = VS.unsafeFromForeignPtr0 fptr giga :: VS.Vector Int32
      n <- randomRIO (0, giga)
      print (vs VS.! n) 
    
  
main :: IO ()  
main = do
  r <- getArgs
  case r !! 0 of
    "0" -> main0
    "1" -> main1
    "2" -> main2
    "3" -> main3
    "4" -> main4
    "5" -> main5
    _ -> error "no such option" 
