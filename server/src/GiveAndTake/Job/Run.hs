{-# LANGUAGE OverloadedRecordDot #-}

module GiveAndTake.Job.Run where

import Database.Persist ((=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.DB
import GiveAndTake.Job.Con
import GiveAndTake.Job.Utils
import GiveAndTake.Media
import GiveAndTake.Prelude
import GiveAndTake.Utils
import GiveAndTake.VerifyEmail (runEmailVerification)
import UnliftIO
import UnliftIO.Concurrent qualified as UC

runJob :: (HasJob m, HasMediaJob m) => GATJob -> m JobResult
runJob (GATJobVerifyEmail val) = runEmailVerification val
runJob (GATJobMediaUpload info) = runMediaJob info

-- FIXME: give jobs time to finish before shutting down
jobRunner :: (HasJob m, HasMediaJob m, MonadLogger m) => Int -> m ()
jobRunner jobLimit = runJobLoop
 where
  runJobLoop = do
    runningJobEnts <- runDB $ P.selectList [JobStatus ==. JobRunning] []
    pendingJobEnts <-
      runDB $
        updateSelect
          [JobStatus ==. JobPending]
          [P.Asc JobCreatedAt, P.LimitTo (jobLimit - length runningJobEnts)]
          [JobStatus =. JobRunning]
    forM_ pendingJobEnts \jobEnt -> do
      UC.forkIO $
        let
          onStart = do
            logInfoN [fmt|Starting job {show @Text jobEnt.val.payload}|]
          onSuccess result = do
            logInfoN [fmt|Finished job {show @Text jobEnt.val.payload} successfully|]
            ct <- getUTCTime
            runDB $
              P.update
                jobEnt.entityKey
                [ JobStatus =. JobFinished
                , JobEndedAt =. Just ct
                , JobResult =. Just result
                ]
          onFailure (e :: SomeException) = do
            -- FIXME: use monadlogger
            logWarnN [fmt|Error running job {show @Text jobEnt.val.payload}: {show @Text e}|]
            ct <- getUTCTime
            runDB $
              P.update
                jobEnt.entityKey
                [ JobStatus =. JobFailed
                , JobEndedAt =. Just ct
                , JobJobError =. Just (show e)
                ]
            throwIO e
         in
          onStart
            >> catch (runJob jobEnt.val.payload >>= onSuccess) onFailure
    waitForNudge
    runJobLoop
