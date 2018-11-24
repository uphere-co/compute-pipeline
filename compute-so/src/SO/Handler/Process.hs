{-# LANGUAGE OverloadedStrings #-}
module SO.Handler.Process
  ( mainProcess
  ) where

import           Control.Concurrent.STM   ( atomically, modifyTVar' )
import           Control.Monad            ( forever )
import           Control.Monad.IO.Class   ( liftIO )
import qualified Data.IntMap as IM
import           Data.Semigroup           ( (<>) )
import           Data.Text                ( Text )
------
import           CloudHaskell.QueryQueue  ( QQVar
                                          , QueryStatus(..)
                                          , waitQuery
                                          )
import           CloudHaskell.Util        ( tellLog )
import           CloudHaskell.Type        ( Pipeline )



-- TODO: need to refactor out this query processing (handleQuery).
mainProcess :: QQVar Text Text -> Pipeline ()
mainProcess qqvar = do
  tellLog "mainProcess2"
  forever $ do
    (i,q) <- liftIO $ atomically $ waitQuery qqvar
    let r = q <> ":1234"
    liftIO $ atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
    tellLog $ "answered with " ++ show r
