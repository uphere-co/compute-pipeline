{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GADTs                      #-}

module JobQueue.JobQueue where

import Control.Applicative
-- import Control.Monad.State
-- import Control.Monad.Reader
import           Data.Aeson
import qualified Data.IntMap as M
import           GHC.Generics (Generic)
--
-- import HEP.Automation.EventGeneration.Type
import Storage.Type
--
import Prelude hiding (length)

-- newtype JobNumber = JobNum { unJobNum :: Int}
type JobNumber = Int

data JobPriority = NonUrgent | Urgent
                 deriving (Show,Eq,Ord,Generic)

instance FromJSON JobPriority
instance ToJSON JobPriority

data JobInfo detail = JobInfo {
  jobinfo_id     :: JobNumber,
  jobinfo_detail :: detail,
  jobinfo_status :: JobStatus,
  jobinfo_priority :: JobPriority,
  jobinfo_dependency :: [JobNumber]
} deriving (Show,Eq,Ord,Generic)

instance (FromJSON detail) => FromJSON (JobInfo detail)
instance (ToJSON detail) => ToJSON (JobInfo detail)

-- type ManyJobInfo detail = [ (Int, JobInfo) ]

data JobStatus = Unassigned
               | Assigned String
               | BeingCalculated String
               | BeingTested String
               | Finished String
               deriving (Show, Eq, Ord, Generic)

instance FromJSON JobStatus
instance ToJSON JobStatus

data JobInfoQueue detail = JobInfoQueue {
                             jobinfoqueue_lastid :: Int,
                             jobinfoqueue_map    :: M.IntMap (JobInfo detail)
                           }
                         deriving (Show,Eq,Ord,Generic)

instance FromJSON detail => FromJSON (JobInfoQueue detail)
instance ToJSON detail => ToJSON (JobInfoQueue detail)


{-
addJob :: JobDetail -> Update JobInfoQueue (Int,JobInfo)
addJob j = addJobWithPriority j NonUrgent


addJobWithPriority :: JobDetail -> JobPriority -> Update JobInfoQueue (Int,JobInfo)
addJobWithPriority j p = do JobInfoQueue lastid m <- get
                            let newjob = JobInfo (lastid+1) j Unassigned p []
                            put $ JobInfoQueue (lastid+1) (M.insert (lastid+1) newjob m)
                            return (lastid+1,newjob)

-}

{-
queryAll :: Query JobInfoQueue (Int, [JobInfo])
queryAll = do JobInfoQueue lastid m <- ask
              return (lastid, M.elems m)

queryJob :: Int -> Query JobInfoQueue (Maybe JobInfo)
queryJob k = do JobInfoQueue _ m <- ask
                return (M.lookup k m)

updateJob :: Int -> JobInfo -> Update JobInfoQueue (Maybe JobInfo)
updateJob k jinfo
    | k /= jobinfo_id jinfo = return Nothing
    | k == jobinfo_id jinfo =
        do JobInfoQueue l m <- get
           let f k a = Just jinfo
           let (r,m') = M.updateLookupWithKey f k m
           put (JobInfoQueue l m')
           return r

deleteJob :: Int -> Update JobInfoQueue ()
deleteJob k = do
  JobInfoQueue l m <- get
  let m' = M.delete k m
  put (JobInfoQueue l m')


-}
