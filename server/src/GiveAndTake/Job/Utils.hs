module GiveAndTake.Job.Utils where

import Data.Typeable
import Database.Persist qualified as P
import Database.Persist.Sql qualified as PS
import GiveAndTake.DB
import GiveAndTake.Job.Con
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import UnliftIO.Exception (throwIO)

type HasJob m = (HasUConfig m, HasDBPool m, MonadIO m, HasJobCon m)

-- withConnection :: (MonadIO m, HasDBPool m) => (PS.Connection -> IO a) -> m a
-- withConnection f = do
--   pool <- askM @(Pool PS.SqlBackend)
--   mRes <- liftIO $ withResource pool $ maybe (pure Nothing) (fmap Just . f) . PS.getSimpleConn
--   case mRes of
--     Nothing -> error "Impossible: PS.getSimpleConn returned Nothing."
--     Just res -> pure res

createJob :: (HasHandler m) => GATJob -> m JobId
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

-- throws Server Error
getByKeyJE ::
  forall (record :: Type) (m :: Type -> Type).
  ( P.PersistEntityBackend record ~ P.BaseBackend PS.SqlBackend
  , MonadIO m
  , P.PersistEntity record
  , HasDBPool m
  , Typeable record
  ) =>
  P.Key record ->
  m record
getByKeyJE key =
  let typeStr = typeName @record
   in runDB (P.get key)
        >>= maybe (throwIO $ JobError [fmt|{typeStr} with key {show @Text key} not found|]) pure
