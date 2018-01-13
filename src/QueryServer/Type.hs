{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryServer.Type where

{-
import           Control.Applicative    ((<|>))
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.Binary      as Bi
import           Data.Text              (Text)

data Query = QueryText { query_text :: Text, query_tables :: [Text] }
           | QueryRegister { query_register :: Text }
           --  | QueryById { query_ids :: [Int] }
           | QuerySuggest { query_ideas :: [Text] }
  deriving (Show,Eq,Ord)

instance Bi.Binary Query where
  put (QueryText txt tables) = Bi.put (0 :: Int) >> Bi.put txt >> Bi.put tables
  put (QueryRegister txt) = Bi.put (1 :: Int) >> Bi.put txt
  -- put (QueryById     ids) = Bi.put (2 :: Int) >> Bi.put ids
  put (QuerySuggest txts) = Bi.put (3 :: Int) >> Bi.put txts

  get = do
    t :: Int <- Bi.get
    case t of
      0 -> QueryText <$> Bi.get <*> Bi.get
      1 -> QueryRegister <$> Bi.get
      -- 2 -> QueryById     <$> Bi.get
      3 -> QuerySuggest <$> Bi.get
      _ -> fail "Query: no such type"


data RegisteredSentences = RS { rs_sent_uids :: [ Int ]
                              , rs_Countries :: [ Text ]
                              , rs_confine_ygp_table_columns :: [ Text ]
                              , rs_max_clip_len :: Maybe Int
                              }
  deriving (Show,Eq,Ord)

instance FromJSON RegisteredSentences where
  parseJSON (Object o) = do
    suids <- o .: "sent_uids"
    countries <- (o .: "Countries" <|> return [])
    tablecolumns <- (o .: "confine_ygp_table_columns" <|> return [])
    ((o .: "max_clip_len" >>= \n -> return (RS suids countries tablecolumns (Just n)))
     <|> return (RS suids countries tablecolumns Nothing))
  parseJSON invalid    = typeMismatch "RegisteredSentences" invalid

instance ToJSON RegisteredSentences where
  toJSON RS {..} = object $ [ "sent_uids" .= rs_sent_uids
                            , "Countries" .= rs_Countries
                            , "confine_ygp_table_columns" .= rs_confine_ygp_table_columns
                            ]
                            ++ maybe [] (\x -> ["max_clip_len" .= x]) rs_max_clip_len


-}
