{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Type where

import           Data.Aeson          (ToJSON(..),object,(.=))
import           Data.Csv     as CSV hiding ((.=))
import           Data.Default        (Default,def)
import           Data.Text           (Text)

-- The definition of numeric reference is following
-- 1. It contains at least one number.
-- 2. It can contain periods and commas.
-- 3. It can contain alphabetical characters.
-- e.g., 137, 3.14, Article 37

data Token = TWord Text
           | Number
           | Period
           | Comma

instance Show Token where
  show (TWord txt) = show txt
  show Number      = show ("Number" :: Text)
  show Period      = show ("." :: Text)
  show Comma       = show ("," :: Text)

type NumEntity = [Token]

data Regulation = Regulation
     { regid       :: Int
     , language    :: Text
     , countrycode :: Text
     , countryname :: Text

     , enmainrequire :: Text
     , reglevel    :: Int
     , statuscode  :: Maybe Int
     , statusname  :: Maybe Text
     , regtitle    :: Text
     } deriving Show

instance FromNamedRecord Regulation where
    parseNamedRecord r = Regulation <$> r .: "regid"
                                    <*> r .: "language"
                                    <*> r .: "countrycode"
                                    <*> r .: "countryname"
                                    <*> r .: "enmainrequire"
                                    <*> r .: "reglevel"
                                    <*> r .: "statuscode"
                                    <*> r .: "statusname"
                                    <*> r .: "regtitle"


bkgFile :: String
bkgFile = "/data/groups/uphere/tmp/news2012-2013"

sigFile :: String
sigFile = "/data/groups/uphere/tmp/regulation.csv"

tblFile :: String
tblFile = "data/tables.txt"

config :: NETaggerConfig
config = def

config2 :: NETaggerConfig
config2 = NETConfig { numericPattern = "#NUM"
                  , namedEntityTag = "#!NumberedReference"
                  , cutoffSNRatio = 10
                  , cutoffSig = 500
                  , cutoffNTop = 30
                  }

data NETaggerConfig = NETConfig { numericPattern :: Text
                                , namedEntityTag :: Text
                                , cutoffSNRatio :: Double
                                , cutoffSig     :: Int
                                , cutoffNTop    :: Int
                                }

type NETTagger = [([Text],(Int,Double))]

instance Default NETaggerConfig where
  def = NETConfig { numericPattern = "#NUM"
                  , namedEntityTag = "#!NumberedReference"
                  , cutoffSNRatio = 3
                  , cutoffSig = 10
                  , cutoffNTop = 30
                  }
nbefore :: Int
nbefore = 20 -- 10

nafter :: Int
nafter = 10 -- 5

maxratio :: Double
maxratio = 1e5

bkgLines :: Int
bkgLines = 1000000 -- 1000000


data Zipper a = Z [a] a [a]

deriving instance (Show a) => Show (Zipper a)

data Zipper' a = Z' [a] [a] [a]

deriving instance (Show a) => Show (Zipper' a)


newtype AnnotText = AnnotText { unAnnotText :: [(Text,Bool)] } deriving (Show)


-- newtype RowData = RowData { unRowData :: [(Int,Int,Text)] } deriving (Show)

data MetaData = MetaData { originalFileName :: Text
                         , rowData :: [OffsetData]
                         } deriving (Show)

data OffsetData = OffsetData { fileName :: Text
                             , begin :: Int
                             , end   :: Int
                             } deriving (Show)

instance ToJSON MetaData where
  toJSON MetaData {..} = object [ "original_file" .= originalFileName, "sentences" .= [object ["sent_file" .= fileName x, "begin" .= begin x, "end" .= end x] | x <- rowData] ] 


{-
{"original_file" : "TestRow.txt",
  "sentences" : [
                    {"sent_file" : "TestRow.txt.11", "begin":1128,"end":1144},
                                    {"sent_file" : "TestRow.txt.14", "begin":1217,"end":1328}
                ]
  }
-}
