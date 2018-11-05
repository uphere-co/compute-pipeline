{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Main1 where

import           Control.Distributed.Process.Lifted
                                     ( expect, send )
import           Control.Error.Util  ( failWith )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Data.Aeson          ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as HM
import           Data.List           ( find )
import           Data.Semigroup      ( (<>) )
import           Data.Text           ( Text )
-----------------
import           CloudHaskell.Client ( heartBeatHandshake, routerHandshake )
import           CloudHaskell.Type   ( TCPPort(..)
                                     , Gateway(gatewayMaster)
                                     , MasterConfig(..)
                                     , SlaveConfig(..)
                                     , handleError
                                     )
import           CloudHaskell.Util   ( lookupRouter )
-----------------
import           Compute             ( masterMain, slaveMain )
import           Compute.Type.Status ( Status(..) )

import Worker.Type



{-
workerMain :: IO ()
workerMain =
  slaveMain (mConfig,sConfig)
    -- TODO: this is not a correct implementation. we should change it. (Why?)
    (\gw -> do
       heartBeatHandshake (gatewayMaster gw) $
         routerHandshake $ \router -> do
           themaster <- lookupRouter "master" router
           send themaster cname
           () <- expect -- this is a kill signal.
           pure ()
    )
-}
