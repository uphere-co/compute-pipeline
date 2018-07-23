{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Aeson                (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as HM
import           Data.Monoid               ((<>))
import           Options.Applicative
--
import           CloudHaskell.Type         (TCPPort(..))
--
import           SemanticParserAPI.Compute (computeMain)
import           SemanticParserAPI.Compute.Type (ComputeConfig(..),NetworkConfig(..)
                                                ,CellConfig(..))


data ComputeServerOption = ComputeServerOption {
                             servLangConfig :: FilePath
                           , servComputeConfig :: FilePath
                           }
                         deriving (Show,Eq,Ord)

pOptions :: Parser ComputeServerOption
pOptions = ComputeServerOption
           <$> strOption (long "lang" <> short 'l' <> help "Language engine configuration")
           <*> strOption (long "compute" <> short 'c' <> help "Compute pipeline configuration")

computeServerOption :: ParserInfo ComputeServerOption
computeServerOption = info pOptions ( fullDesc <>
                                      progDesc "semantic-parser-api-compute server daemon"
                                    )


main :: IO ()
main = do
  opt <- execParser computeServerOption
  ecompcfg :: Either String ComputeConfig <-
    eitherDecodeStrict <$> B.readFile (servComputeConfig opt)
  case ecompcfg of
    Right compcfg -> do
      let hostGlobalIP = hostg (computeServer compcfg)
          hostLocalIP = hostl (computeServer compcfg)
          hostPort = port (computeServer compcfg)
          bypassNER = computeBypassNER compcfg
          bypassTEXTNER = computeBypassTEXTNER compcfg
          initStatus = HM.fromList $ map (\c -> (cellName c,Nothing))  (computeCells compcfg)
      computeMain
        initStatus
        (TCPPort hostPort,hostGlobalIP,hostLocalIP)
        (bypassNER,bypassTEXTNER)
        (servLangConfig opt)
    Left err -> print err
