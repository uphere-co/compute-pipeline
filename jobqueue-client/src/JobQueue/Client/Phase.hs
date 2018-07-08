{-# LANGUAGE ScopedTypeVariables #-}
module JobQueue.Client.Phase where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Trans
-- import Control.Monad.Trans.Either
import Data.Aeson.Types
--
import JobQueue.JobQueue
import JobQueue.Config
import JobQueue.Client.Job
import Storage.Type

{-
-- |
guardEither :: (Monad m) => String -> Bool -> EitherT String m () -> EitherT String m ()
guardEither msg b action = if b then action else left msg


-- |
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right

-}

{-
-- |
startListPhase :: URL -> String -> IO ()
startListPhase url qtyp = do
  result :: Either String [JobInfo]
    <- case qtyp of
         "all"        -> getJsonFromServer url "queuelist"
         "unassigned" -> getJsonFromServer url "queuelist/unassigned"
         "inprogress" -> getJsonFromServer url "queuelist/inprogress"
         "finished"   -> getJsonFromServer url "queuelist/finished"
         _ -> return (Left "No such option")
  print result

-- |
startGetPhase :: URL -> Int -> IO ()
startGetPhase url jid = jobqueueGet url jid >>= print

-- |
startDeletePhase :: URL -> Int -> IO ()
startDeletePhase url jid = do
  j <- jobqueueGet url jid
  case j of
    Left err -> putStrLn $ "error : " ++ err
    Right jobinfo -> do putStrLn $ " job is " ++ show jobinfo
                        jobqueueDelete url jid
                        return ()

startWaitPhase :: (ClientConfiguration,EventgenConfig) -> URL -> Int -> Int -> IO ()
startWaitPhase (cc,ec) url n assignfailure = do
  putStrLn "starting Wait Phase"
  when (assignfailure < 0) $ do
      putStrLn "too many assign failure. kill the process"
      error "assign failure kill"
  newn <- if (n < 0) then do putStrLn "too many failure, need to take a rest"
                             threadDelay (60*1000000)
                             return 3
                     else return n
  r <- jobqueueAssign url cc
  case r of
    Right jinfo -> startJobPhase (cc,ec) url jinfo newn 10
    Left err -> do
      putStrLn ("assign failure : " ++ err)
      putStrLn ("remaining chance : " ++ show assignfailure)
      -- threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      -- threadDelay (1000000*60)
      threadDelay ( 1000000*10 )
      startWaitPhase (cc,ec) url newn (assignfailure-1)


testactiontrue :: (Monad m) => EitherT String m ()
testactiontrue = right ()

testactionfalse :: (Monad m) => EitherT String m ()
testactionfalse = left "error"


startJobPhase :: (ClientConfiguration,EventgenConfig) -> URL -> JobInfo -> Int -> Int -> IO ()
startJobPhase (cc,ec) url jinfo n af = do
  putStrLn "starting Job Phase"
  let cname = computerName cc
      ss = evgen_scriptsetup ec
  -- check job here

  r <- confirmAssignment url cname jinfo
  case r of
    Left err -> putStrLn err >> startWaitPhase (cc,ec) url (n-1) af
    Right jinfo' -> do
      putStrLn "job assigned well"
      r' <- getWebDAVInfo url
      case r' of
        Left err -> startWaitPhase (cc,ec) url n af
        Right sconf -> do
          let -- wc = WorkConfig lc sconf
              -- job = jobMatch jinfo
              failureCallback = backToUnassigned url jinfo >> startWaitPhase (cc,ec) url (n-1) af
          -- putStrLn $ "Work Configuration = " ++ show wc
          -- b1 <- pipeline_checkSystem job wc jinfo'
          print sconf
          let EventGen evset rdir = jobinfo_detail jinfo
          case evset of
            EventSet pset param rset -> do
              let wsetup = WS { ws_ssetup = ss, ws_psetup = pset , ws_param = param, ws_rsetup = rset, ws_storage = rdir }
              r'' <- runEitherT $ do
                testactiontrue -- checkSystem
                liftIO $ threadDelay 3000000
                lift $ changeStatus url jinfo' (BeingCalculated cname)
                liftIO $ work wsetup

                -- testactiontrue -- startWork
                liftIO $ threadDelay 3000000
                lift $ changeStatus url jinfo' (BeingTested cname)
                -- testactionfalse -- startTest
                -- liftIO $ threadDelay 3000000
                let uploadtyp = uploadhep rset
                    whost = evgen_webdavroot ec
                    pkey = evgen_privatekeyfile ec
                    pswd = evgen_passwordstore ec
                cr <- EitherT (maybeToEither "cannot retrieve credentials" <$> getCredential pkey pswd)
                let wdavcfg = WebDAVConfig { webdav_credential = cr, webdav_baseurl = whost }
                liftIO $ uploadEventFull uploadtyp wdavcfg wsetup
                liftIO $ uploadJSON wdavcfg wsetup
                liftIO $ threadDelay 3000000
                lift $ changeStatus url jinfo' (Finished cname)
                liftIO $ threadDelay 3000000
                return ()
              either (\msg -> putStrLn msg >> failureCallback) (const (return ())) r''
  startWaitPhase (cc,ec) url n af
-}
