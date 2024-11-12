module GiveAndTake.VerifyEmail where

import Data.Time (addUTCTime, nominalDay)
import Data.Time.Clock (diffUTCTime)
import Database.Persist ((+=.), (=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.Auth (updateFullyAuthenticated)
import GiveAndTake.DB
import GiveAndTake.Email
import GiveAndTake.Job.Utils (HasJob, createJob)
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Network.Mail.SMTP qualified as MS
import Servant qualified as S
import UnliftIO.Exception

startVerifyEmailJob ::
  (HasHandler m) =>
  UserId ->
  Doc "UserName" Text ->
  Doc "Email" Text ->
  ECReason ->
  m JobId
startVerifyEmailJob userId userName userEmail reason = do
  emailConfs <- runDB $ P.selectList @EmailConfirm [#email ==. userEmail] []
  ct <- getUTCTime
  case filter ((.val.isConfirmed)) emailConfs of
    [x]
      | x.val.user == userId -> throwError S.err409{S.errBody = "Email already confirmed."}
      | otherwise -> throwError S.err409{S.errBody = "Email already confirmed by another user."}
    []
      | length (filter (\x -> x.val.user == userId && x.val.email == userEmail) emailConfs) >= 3
          && any (\conf -> conf.val.sentAt `diffUTCTime` ct <= nominalDay) emailConfs ->
          throwError S.err409{S.errBody = "Email verification limit reached for now."}
      | otherwise -> startJob
    _ -> throwError S.err500{S.errBody = "Multiple successfull email verifications found."}
 where
  startJob = do
    -- send email
    secret <- randomUrlToken
    ct <- getUTCTime
    secretHash <- hashToken secret
    verifId <-
      runDB $
        insertUUID
          EmailConfirm
            { user = userId
            , email = userEmail
            , isConfirmed = False
            , secretHash
            , confirmedAt = Nothing
            , reason
            , sentAt = ct
            }
    jobId <-
      createJob $
        GATJobVerifyEmail
          GATJobVerifyEmailData
            { id = verifId
            , secret
            , userName
            , userEmail
            }
    pure jobId

runEmailVerification :: (HasJob m) => GATJobVerifyEmailData -> m JobResult
runEmailVerification val = do
  uconfig <- askM @UConfig

  mEmailConf <- runDB (P.get val.id)
  emailConf <- maybe (throwIO $ JobError [fmt|Failed to get EmailConf with key {val.id}|]) pure mEmailConf

  let url = authUrl uconfig [[fmt|verifyemail?secret={val.secret}&id={val.id}|]]
      bodyReason :: Text = case emailConf.reason of
        ECReasonSignup -> "recently created an account"
        ECReasonChangeEmail -> "tried to change your email address"
      plainBody =
        [fmtTrim|
             Hello {val.userName}!

             Please verify your email address for {uconfig.serviceName} by clicking on the following link.
             Do not click this link if you have not {bodyReason} at {uconfig.serviceName}.
             {url}
             |]
      subjectReason :: Text = case emailConf.reason of
        ECReasonSignup -> "to sign up for"
        ECReasonChangeEmail -> "to change your email address at"
      subject = [fmt|Verify your email {subjectReason} {uconfig.serviceName}|]
  sendMail
    Mail
      { to = [MS.Address (Just val.userName) val.userEmail]
      , subject
      , plainBody
      , htmlBody = Nothing
      }
  pure $ GATJobResultVerifyEmail ()

finishEmailVerification :: (HasHandler m) => VerifyEmail -> m ()
finishEmailVerification (VerifyEmail{id = verifId, secret = secret}) = do
  eConf <- getByKeySE verifId
  ct <- getUTCTime

  when ((30 * 60) `addUTCTime` eConf.sentAt < ct) $ -- expire after 30 minutes
    throwError S.err409{S.errBody = "Email confirmation expired."}

  if validateToken secret eConf.secretHash
    then do
      curtime <- getUTCTime

      -- we don't want two seperate calls too succeed
      nextEConf <- getByKeySE verifId
      runDB $ P.update verifId [EmailConfirmIsConfirmed =. True]
      if nextEConf.isConfirmed
        then throwError S.err409{S.errBody = "Email confirmation already confirmed."}
        else do
          runDB $
            P.update
              verifId
              [ #confirmedAt =. Just curtime
              ]
          verifyEmailOnSuccess nextEConf
    else throwError S.err406{S.errBody = "Secret does not match."}

verifyEmailOnSuccess :: (HasHandler m) => EmailConfirm -> m ()
verifyEmailOnSuccess emailConfirm = do
  case emailConfirm.reason of
    ECReasonSignup -> updateFullyAuthenticated emailConfirm.user
    ECReasonChangeEmail -> runDB $ P.update emailConfirm.user [#email =. emailConfirm.email]
