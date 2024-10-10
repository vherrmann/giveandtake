module GiveAndTake.Handlers.Job where

import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils (checkIsEqUser)
import GiveAndTake.Prelude
import GiveAndTake.Types
import Servant (type (:<|>) (..))
import Servant qualified as S

checkAuthJob :: (HasHandler m) => UserUUID -> GATJob -> m ()
checkAuthJob userId = \case
  (GATJobVerifyEmail val) -> checkIsEqUser userId val.userId
  (GATJobMediaUpload val) -> checkIsEqUser userId val.userId

getStatusJobH :: (HasHandler m) => UserUUID -> JobUUID -> m JobStatus
getStatusJobH userId jobId = do
  job <- getByKeySE @Job jobId
  checkAuthJob userId job.payload
  pure job.status

-- cancelJobH :: (HasHandler m) => UserUUID -> JobUUID -> m ()
-- cancelJobH userId jobId = do
--   job <- getByKeySE @Job jobId
--   checkAuthJob userId job.payload
--   cancelJob job

-- FIXME: remove this, since it's useless?
verifyEmailResJobH :: UserUUID -> JobUUID -> RHandler m ()
verifyEmailResJobH userId jobId = do
  -- FIXME: remove boilerplate auth etc.
  job <- getByKeySE @Job jobId
  checkAuthJob userId job.payload
  pure ()

mediaCompressResJobH :: UserUUID -> JobUUID -> RHandler m UploadMediaResponse
mediaCompressResJobH userId jobId = do
  job <- getByKeySE @Job jobId
  checkAuthJob userId job.payload
  case job.result of
    Just (GATJobResultMediaUpload res) -> pure $ UploadMediaResponse res
    Nothing -> throwError S.err404{S.errBody = "Job result not found."}
    _ -> throwError S.err500{S.errBody = "Job result has wrong format."}

jobHandler :: Entity User -> RServer m JobApi
jobHandler userEnt =
  getStatusJobH userEnt.key
    :<|> ( \jobId ->
            verifyEmailResJobH userEnt.key jobId
              :<|> mediaCompressResJobH userEnt.key jobId
         )

-- :<|> cancelJobH userEnt.key
