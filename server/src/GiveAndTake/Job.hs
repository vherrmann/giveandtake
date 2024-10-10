{-# LANGUAGE OverloadedRecordDot #-}

module GiveAndTake.Job where

import Data.Text.IO qualified as T
import Database.Persist ((=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Email
import GiveAndTake.JobCon
import GiveAndTake.Media
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Network.Mail.SMTP qualified as MS
import UnliftIO
import UnliftIO.Concurrent qualified as UC

type HasJob m = (HasUConfig m, HasDBPool m, MonadIO m, HasJobCon m)

-- withConnection :: (MonadIO m, HasDBPool m) => (PS.Connection -> IO a) -> m a
-- withConnection f = do
--   pool <- askM @(Pool PS.SqlBackend)
--   mRes <- liftIO $ withResource pool $ maybe (pure Nothing) (fmap Just . f) . PS.getSimpleConn
--   case mRes of
--     Nothing -> error "Impossible: PS.getSimpleConn returned Nothing."
--     Just res -> pure res

createJob :: (HasHandler m) => GATJob -> m JobUUID
createJob gatjob = do
  ct <- getUTCTime
  -- FIXME: start job
  jobId <-
    runDB $
      insertUUID
        Job
          { payload = gatjob
          , status = JobPending
          , createdAt = ct
          , startedAt = Nothing
          , endedAt = Nothing
          , jobError = Nothing
          , result = Nothing
          }
  nudgeJobRunner
  pure jobId

-- FIXME: report success/failure, Maybe?
cancelJob :: (HasHandler m) => Job -> m ()
cancelJob job = todo

runJob :: (HasJob m, HasMediaJob m) => GATJob -> m JobResult
runJob (GATJobVerifyEmail val) = do
  uconfig <- askM @UConfig
  let url = authUrl uconfig [[fmt|verifyemail?secret={val.secret}&userId={val.userId}|]]
      emailPlainText =
        [fmtTrim|
             Hello {val.userName}!

             Please verify your email address for {uconfig.serviceName} by clicking on the following link:
             Do not click this link if you have not created an account at {uconfig.serviceName}.
             {url}
             |]
  sendMail
    Mail
      { to = [MS.Address (Just val.userName) val.userEmail]
      , subject = "Verify your email"
      , plainBody = emailPlainText
      , htmlBody = Nothing
      }
  pure $ GATJobResultVerifyEmail ()
runJob (GATJobMediaUpload info) = runMediaJob info

-- FIXME: give jobs time to finish before shutting down
jobRunner :: (HasJob m, HasMediaJob m, MonadUnliftIO m) => Int -> m ()
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
          onSuccess result = do
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
            liftIO $ T.hPutStrLn stderr [fmt|Error running job {show @Text jobEnt.val.payload}: {show @Text e}|]
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
          catch (runJob jobEnt.val.payload >>= onSuccess) onFailure
    waitForNudge
    runJobLoop
