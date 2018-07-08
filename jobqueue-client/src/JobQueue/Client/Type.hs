{-# LANGUAGE DeriveDataTypeable #-}
module JobQueue.Client.Type where

data JobClient = Get    { jobid :: Int, url  :: String }
               | List   { queuetyp :: String, url :: String }
               | Start  { config :: FilePath }
               | StartTest { config :: FilePath }
               | Revert { jobid :: Int, config :: FilePath }
               | Finish { jobid :: Int, config :: FilePath }
               | Delete { jobid :: Int, config :: FilePath }
               deriving (Show)

get :: JobClient
get = Get { jobid = 0 &= typ "JOBID" &= argPos 0
          , url = def &= argPos 1}

list :: JobClient
list = List { queuetyp = "all" &= typ "QUEUETYP" &= argPos 0
            , url = def &= argPos 1}
start :: JobClient
start = Start { config = "test.conf" }

starttest :: JobClient
starttest = StartTest { config = "test.conf" }

revert :: JobClient
revert = Revert { jobid = 0 &= typ "JOBID" &= argPos 0
                , config = "test.conf" }

finish :: JobClient
finish = Finish { jobid = 0 &= typ "JOBID" &= argPos 0
                , config = "test.conf" }


delete :: JobClient
delete = Delete { jobid = 0 &= typ "JOBID" &= argPos 0
                , config = "test.conf" }


-- mode :: JobClient
-- mode = modes [get, list, start, starttest, revert, finish, delete]
