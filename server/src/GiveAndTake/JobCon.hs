module GiveAndTake.JobCon where

import GiveAndTake.Prelude
import UnliftIO.STM

newtype JobCon = JobCon (TMVar ())

type HasJobCon m = MonadReaderM JobCon m

createJobCon :: (MonadIO m) => m JobCon
createJobCon = fmap JobCon newEmptyTMVarIO

nudgeJobRunner :: (HasJobCon m, MonadIO m) => m ()
nudgeJobRunner = do
  JobCon tmv <- askM @JobCon
  atomically $ writeTMVar tmv ()

waitForNudge :: (HasJobCon m, MonadIO m) => m ()
waitForNudge = do
  JobCon tmv <- askM @JobCon
  atomically $ takeTMVar tmv
