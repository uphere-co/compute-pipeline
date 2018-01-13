{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Query.Type where

import           Control.Lens
import           Data.Monoid              ((<>))
import           Options.Applicative
import qualified Options.Applicative as O


data ProgOption = ProgOption { _configpath :: FilePath
                             , _port       :: Int
                             } deriving Show

makeLenses ''ProgOption

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "config" <> short 'c' <> help "Config JSON path")
                      <*> O.option O.auto (O.long "port" <> O.short 'p' <> O.help "port")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "NLP Pipeline")
